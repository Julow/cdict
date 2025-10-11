module Trie = struct
  (** A simple trie implementation that can be created from a list of words. *)

  type 'a t = { leaf : 'a option; branches : (char * 'a t) list }
  (** Branches are sorted in lexicographically ascending order. *)

  let group_by_first_char prefix_len words =
    let rec start acc leaf = function
      | [] -> (List.rev acc, leaf)
      | (w, leaf) :: tl when String.length w <= prefix_len ->
          start acc (Some leaf) tl
      | ((w, _) as w') :: tl -> node acc leaf w.[prefix_len] [ w' ] tl
    and node acc leaf char group = function
      | ((w, _) as w') :: tl
        when String.length w > prefix_len && w.[prefix_len] = char ->
          node acc leaf char (w' :: group) tl
      | lst -> start ((char, List.rev group) :: acc) leaf lst
    in
    start [] None words

  let rec t_of_list prefix_len words =
    let branches, leaf = group_by_first_char prefix_len words in
    let branches = List.map (t_of_branch (prefix_len + 1)) branches in
    { leaf; branches }

  and t_of_branch prefix_len (c, words) = (c, t_of_list prefix_len words)

  let of_list words =
    List.sort_uniq (fun (a, _) (b, _) -> String.compare a b) words
    |> t_of_list 0
end

module Optimized = struct
  (** Encode a trie into a form closer to libcdict's format, taking advantages
      of possible optimisations. *)

  module Id : sig
    type t

    val zero : t
    val fresh : unit -> t
    val compare : t -> t -> int
  end = struct
    type t = int

    let _uniq = ref 0
    let zero = 0

    let fresh () =
      incr _uniq;
      !_uniq (* Starts at 1. *)

    let compare = Int.compare
  end

  module IdMap = Map.Make (Id)

  type 'a branches = {
    b_next : 'a branches option;  (** Sorted *)
    b_branches : (char * Id.t) list;  (** Sorted *)
  }

  type 'a node =
    | Leaf of 'a
    | Branches of { leaf : 'a option; branches : 'a branches }
    | Prefix of string * Id.t  (** 1 to 4 bytes prefix *)

  type 'a t = 'a node IdMap.t

  let add ids node =
    let id = Id.fresh () in
    (IdMap.add id node ids, id)

  let find = IdMap.find

  let rec fold_prefix prefix trie =
    if String.length prefix >= 4 then (prefix, trie)
    else
      match trie with
      | { Trie.leaf = None; branches = [ (c, next) ] } ->
          fold_prefix (prefix ^ String.make 1 c) next
      | _ -> (prefix, trie)

  (** Split a branches node if it contains a hole bigger than this. *)
  let branch_partition_cutoff =
    let split_cost =
      (* Branches node header + arbitrary value representing the runtime cost
         of traversing to an other node. *)
      8 + 8
    in
    let branch_cost = 4 in
    split_cost / branch_cost

  let rec optimise_branches left = function
    | ((a, _) as a') :: ((b, _) :: _ as tl)
      when Char.code b - Char.code a > branch_partition_cutoff ->
        let b_next = Some (optimise_branches [] tl) in
        { b_branches = List.rev (a' :: left); b_next }
    | a' :: tl -> optimise_branches (a' :: left) tl
    | [] -> { b_branches = List.rev left; b_next = None }

  let rec node_of_trie ids = function
    | { Trie.leaf = Some leaf; branches = [] } -> add ids (Leaf leaf)
    | { leaf = None; branches = [ (c, next) ] } ->
        let prefix, next = fold_prefix (String.make 1 c) next in
        let ids, next = node_of_trie ids next in
        add ids (Prefix (prefix, next))
    | trie -> branches_of_trie (Id.fresh ()) ids trie

  (** Take the id as argument for making sure that the root node has id [0] and
      that branch nodes have an id smaller than their children. *)
  and branches_of_trie node_id ids { leaf; branches } =
    let ids, branches =
      List.fold_right
        (fun (c, next) (ids, acc) ->
          let ids, next = node_of_trie ids next in
          (ids, (c, next) :: acc))
        branches (ids, [])
    in
    assert (branches = List.sort compare branches);
    let ids, branches = (ids, optimise_branches [] branches) in
    (IdMap.add node_id (Branches { leaf; branches }) ids, node_id)

  let of_trie trie = fst (branches_of_trie Id.zero IdMap.empty trie)
end

type 'a t = 'a Optimized.t

let of_list words = Trie.of_list words |> Optimized.of_trie

(*

Trie.of_list ["manges", 1; "manger", 2; "mangue", 3; "structure", 4; "structures", 5]
|> Optimized.of_trie |> Optimized.IdMap.bindings ;;

*)

module Buf = struct
  type t = { mutable b : bytes; mutable end_ : int }
  (** Not using [Buffer] because we need to write back into already written data
      as we want to place some nodes before others. *)

  let create initial_size = { b = Bytes.create initial_size; end_ = 0 }
  let to_string b = Bytes.sub_string b.b 0 b.end_
  let output out_chan b = Out_channel.output out_chan b.b 0 b.end_

  module Open = struct
    let w_int32 b off i = Bytes.set_int32_le b.b off i
    let w_int8 b off i = Bytes.set_uint8 b.b off i
  end

  include Open
end

module Ptr = struct
  type kind = [ `Leaf | `Prefix | `Branches ]
  type t = Ptr of kind * int32

  let v kind ptr = Ptr (kind, Int32.of_int ptr)
  let v_leaf v = Ptr (`Leaf, Int32.(shift_left (of_int v) 3))

  let tag_of_kind = function
    | `Leaf -> 0b001l
    | `Prefix -> 0b010l
    | `Branches -> 0b000l

  let to_int32 (Ptr (kind, ptr)) =
    if not Int32.(logand ptr 0b111l = 0l) then
      failwith "Writing misaligned pointer";
    Int32.(logor (tag_of_kind kind) ptr)

  let w b off t = Buf.w_int32 b off (to_int32 t)
end

module Writer = struct
  open Buf.Open

  let[@inline] align8 offset =
    let down = offset land lnot 7 in
    if down = offset then down else down + 8

  let[@inline] align4 offset =
    let down = offset land lnot 3 in
    if down = offset then down else down + 4

  (** The [align] (defaults to [true]) option allows writing unaligned nodes.
      Unaligned node cannot be referenced with a pointer and [Ptr.w] will raise
      an exception if it encounters one. Unaligned nodes are still aligned to a
      4-bytes boundary, as that's a requirement to make the C code efficient.
      The C code never does 64-bits access into the dictionary. *)
  let alloc ?(align = true) b n =
    let off = b.Buf.end_ in
    let off = if align then align8 off else align4 off in
    let end_ = off + n in
    b.end_ <- end_;
    let b_len = Bytes.length b.b in
    if end_ > b_len then (
      let newb = Bytes.create (end_ * 2) in
      Bytes.blit b.b 0 newb 0 b_len;
      b.b <- newb);
    off

  let rec write_node ?align b ~alloc_leaf nodes id =
    match Optimized.find id nodes with
    (* | exception Not_found -> . *)
    | Optimized.Leaf leaf ->
        (* Leaf nodes cannot be misaligned because pointers to it must be
           written in the dictionary (the pointer is the leaf data). *)
        if Option.is_some align then failwith "Misaligned leaf node";
        alloc_leaf leaf
    | Prefix (p, next) -> write_prefix_node ?align b ~alloc_leaf nodes p next
    | Branches { leaf; branches } ->
        write_branches_node ?align b ~alloc_leaf nodes leaf branches

  and write_prefix_node ?align b ~alloc_leaf nodes p next =
    let off = alloc ?align b 8 in
    let next = write_node b ~alloc_leaf nodes next in
    assert (String.length p <= 4);
    for i = 0 to String.length p - 1 do
      w_int8 b (off + i) (Char.code p.[i])
    done;
    Ptr.w b (off + 4) next;
    Ptr.v `Prefix off

  and write_branches_node ?align b ~alloc_leaf nodes leaf brs =
    let min_value, max_value =
      match brs.b_branches with
      | [] -> (0, 0)
      | (hd, _) :: tl ->
          (Char.code hd, Char.code (List.fold_left (fun _ (c, _) -> c) hd tl))
    in
    assert (min_value <= max_value);
    let len = max_value - min_value + 1 in
    (* Allocate the branches node then write the "next" node right after to
       ensure that they are continuous. Disable alignment to avoid inserting
       padding before a 'next' branches node. Alignment is done explicitly in
       [write_node]. *)
    let off = alloc ?align b (8 + (len * 4)) in
    let branch_off n = off + 8 + (n * 4) in
    let has_next =
      match brs.b_next with
      | Some next ->
          ignore
            (write_branches_node ~align:false b ~alloc_leaf nodes None next);
          1
      | None -> 0
    in
    List.iter
      (fun (c, id) ->
        let c = Char.code c in
        Ptr.w b (branch_off (c - min_value)) (write_node b ~alloc_leaf nodes id))
      brs.b_branches;
    let leaf =
      match leaf with Some leaf -> Ptr.to_int32 (alloc_leaf leaf) | None -> 0l
    in
    w_int32 b off leaf;
    (* Tagged pointer or NULL *)
    w_int8 b (off + 4) min_value;
    w_int8 b (off + 5) len;
    w_int8 b (off + 6) has_next;
    Ptr.v `Branches off

  let write_tree b ~encode_leaf nodes =
    let alloc_leaf leaf = Ptr.v_leaf (encode_leaf leaf) in
    ignore (write_node b ~alloc_leaf nodes Optimized.Id.zero)
end

let to_buf tree ~encode_leaf =
  let b = Buf.create 1_000_000 in
  Writer.write_tree b ~encode_leaf tree;
  b

let to_string tree ~encode_leaf =
  let b = to_buf tree ~encode_leaf in
  Buf.to_string b

let output tree ~encode_leaf out_chan =
  let b = to_buf tree ~encode_leaf in
  Buf.output out_chan b
