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

  let prefix_node_max_length = 3

  module Id : sig
    type t = private int

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
    | Btree of { leaf : 'a option; labels : string; branches : Id.t array }
    | Prefix of string * Id.t  (** 1 to 4 bytes prefix *)

  type 'a t = 'a node IdMap.t

  let add ids node =
    let id = Id.fresh () in
    (IdMap.add id node ids, id)

  let find = IdMap.find

  let rec branches_to_list b =
    b.b_branches :: Option.fold ~none:[] ~some:branches_to_list b.b_next

  let rec fold_prefix prefix trie =
    if String.length prefix >= prefix_node_max_length then (prefix, trie)
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
      4 + 12
    in
    let branch_cost = 4 in
    split_cost / branch_cost

  (** The number of chained [branches] nodes at which a Btree node is
      preferable. *)
  let branches_btree_cutoff = 2

  (** Split a branches node into a chain of smaller branches nodes with holes
      removed. *)
  let rec optimise_branches left = function
    | ((a, _) as a') :: ((b, _) :: _ as tl)
      when Char.code b - Char.code a > branch_partition_cutoff ->
        let b_next = Some (optimise_branches [] tl) in
        { b_branches = List.rev (a' :: left); b_next }
    | a' :: tl -> optimise_branches (a' :: left) tl
    | [] -> { b_branches = List.rev left; b_next = None }

  (** Turn a branches node into a btree node if it's possible and would improve
      performances. *)
  let maybe_branches_to_btree leaf b =
    let b = branches_to_list b in
    let n_branches = List.length b in
    let b = List.concat b in
    let len = List.length b in
    let has_nul_label = match b with ('\000', _) :: _ -> true | _ -> false in
    (* Btree nodes can't have more than 8 branches and can't represent a NUL
       label. Don't make a Btree node if the branches node has few 'next'
       nodes. *)
    if len > 8 || n_branches < branches_btree_cutoff || has_nul_label then None
    else
      let tree = Complete_tree.(to_array (of_sorted_list b)) in
      let labels =
        String.init 8 (fun i ->
            if i < Array.length tree then fst tree.(i) else '\000')
      in
      let branches = Array.map snd tree in
      Some (Btree { leaf; labels; branches })

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
    let branches = optimise_branches [] branches in
    let node =
      match maybe_branches_to_btree leaf branches with
      | Some n -> n
      | None -> Branches { leaf; branches }
    in
    (IdMap.add node_id node ids, node_id)

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
    let w_str b off s = Bytes.blit_string s 0 b.b off (String.length s)
  end

  include Open
end

module Ptr = struct
  type kind =
    [ `Leaf
    | `Prefix
    | `Branches
    | `Branches_with_leaf
    | `Btree
    | `Btree_with_leaf ]

  type t = Ptr of kind * int32

  let v kind ptr = Ptr (kind, Int32.of_int ptr)
  let v_leaf v = Ptr (`Leaf, Int32.(shift_left (of_int v) 3))
  let offset (Ptr (_, p)) = Int32.to_int p

  let tag_of_kind = function
    | `Leaf -> 0b001l
    | `Prefix -> 0b010l
    | `Branches -> 0b000l
    | `Branches_with_leaf -> 0b011l
    | `Btree -> 0b100l
    | `Btree_with_leaf -> 0b101l

  let tag (Ptr (kind, _)) = Int32.to_int (tag_of_kind kind)

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
        if align = Some false then failwith "Misaligned leaf node";
        alloc_leaf leaf
    | Prefix (p, next) -> write_prefix_node ?align b ~alloc_leaf nodes p next
    | Branches { leaf = Some leaf; branches } ->
        write_branches_with_leaf_node ?align b ~alloc_leaf nodes leaf branches
    | Branches { leaf = None; branches } ->
        write_branches_node ?align b ~alloc_leaf nodes branches
    | Btree { leaf = Some leaf; labels; branches } ->
        write_btree_node_with_leaf ?align b ~alloc_leaf nodes leaf labels
          branches
    | Btree { leaf = None; labels; branches } ->
        write_btree_node ?align b ~alloc_leaf nodes labels branches

  and write_prefix_node ?align b ~alloc_leaf nodes p next =
    let off = alloc ?align b 4 in
    (* Special representation when 'next' is a Leaf node. *)
    let next_ptr =
      match Optimized.find next nodes with
      | Optimized.Leaf leaf ->
          let leaf_ptr_off = alloc ~align:false b 4 in
          let leaf_ptr = alloc_leaf leaf in
          Ptr.w b leaf_ptr_off leaf_ptr;
          leaf_ptr
      | _ -> write_node ~align:false b ~alloc_leaf nodes next
    in
    assert (String.length p <= Optimized.prefix_node_max_length);
    for i = 0 to String.length p - 1 do
      w_int8 b (off + i) (Char.code p.[i])
    done;
    w_int8 b (off + Optimized.prefix_node_max_length) (Ptr.tag next_ptr);
    Ptr.v `Prefix off

  and write_branches_node ?align b ~alloc_leaf nodes brs =
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
    let off = alloc ?align b (4 + (len * 4)) in
    let branch_off n = off + 4 + (n * 4) in
    let has_next =
      match brs.b_next with
      | Some next ->
          ignore (write_branches_node ~align:false b ~alloc_leaf nodes next);
          1
      | None -> 0
    in
    List.iter
      (fun (c, id) ->
        let c = Char.code c in
        Ptr.w b (branch_off (c - min_value)) (write_node b ~alloc_leaf nodes id))
      brs.b_branches;
    w_int8 b off min_value;
    w_int8 b (off + 1) len;
    w_int8 b (off + 2) has_next;
    Ptr.v `Branches off

  and write_branches_with_leaf_node ?align b ~alloc_leaf nodes leaf brs =
    let off = alloc ?align b 4 in
    let next_off = write_branches_node ~align:false b ~alloc_leaf nodes brs in
    assert (Ptr.offset next_off = off + 4);
    Ptr.w b off (alloc_leaf leaf);
    Ptr.v `Branches_with_leaf off

  and write_btree_node_with_leaf ?align b ~alloc_leaf nodes leaf labels brs =
    let off = alloc ?align b 4 in
    let b_off = write_btree_node ~align:false b ~alloc_leaf nodes labels brs in
    assert (Ptr.offset b_off = off + 4);
    Ptr.w b off (alloc_leaf leaf);
    Ptr.v `Btree_with_leaf off

  and write_btree_node ?align b ~alloc_leaf nodes labels brs =
    let off = alloc ?align b (8 + (Array.length brs * 4)) in
    w_str b off labels;
    Array.iteri
      (fun i n ->
        Ptr.w b (off + 8 + (i * 4)) (write_node b ~alloc_leaf nodes n))
      brs;
    Ptr.v `Btree off

  let write_tree b ~encode_leaf nodes =
    let alloc_leaf leaf = Ptr.v_leaf (encode_leaf leaf) in
    let header_off = alloc b 4 in
    assert (header_off = 0);
    let root_ptr = write_node b ~alloc_leaf nodes Optimized.Id.zero in
    Ptr.w b header_off root_ptr
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

let hist (type a) to_s key ppf lst =
  let module M = Map.Make (struct
    type t = a

    let compare = compare
  end) in
  Format.fprintf ppf "@[<hov 0>|";
  List.fold_left
    (fun h e ->
      let k = key e in
      let n = try M.find k h + 1 with Not_found -> 1 in
      M.add k n h)
    M.empty lst
  |> M.iter (fun k c -> Format.fprintf ppf " %2s: %-4d@ |" (to_s k) c);
  Format.fprintf ppf "@]"

let hist_int key ppf lst = hist string_of_int key ppf lst
let hist_str key ppf lst = hist Fun.id key ppf lst

let stats ppf tree =
  let open Optimized in
  let count f lst =
    List.fold_left (fun n e -> if f e then n + 1 else n) 0 lst
  in
  let nodes = IdMap.fold (fun _ n acc -> n :: acc) tree [] in
  Format.fprintf ppf "Nodes: %d@\nLeaf nodes: %d@\n" (List.length nodes)
    (count (function Leaf _ -> true | _ -> false) nodes);
  let branches =
    List.filter_map
      (function Branches b -> Some (b.leaf, b.branches) | _ -> None)
      nodes
  in
  let ranges =
    List.concat_map (fun (_, b) -> Optimized.branches_to_list b) branches
  in
  Format.fprintf ppf
    "@[<v 2>Branch nodes: %d@ With leaf: %d@ With 'next' nodes:@ %a@ @[<v \
     2>Ranges: %d:@ %a@]@]@\n"
    (List.length branches)
    (count (fun (l, _) -> Option.is_some l) branches)
    (hist_int (fun (_, b) -> List.length (Optimized.branches_to_list b)))
    branches (List.length ranges) (hist_int List.length) ranges;
  let prefixes =
    List.filter_map
      (function Prefix (p, id) -> Some (p, id) | _ -> None)
      nodes
  in
  Format.fprintf ppf
    "@[<v 2>Prefix nodes: %d@ Followed by:@ %a@ With size:@ %a@]@\n"
    (List.length prefixes)
    (hist_str (fun (_, next) ->
         match IdMap.find next tree with
         | Prefix _ -> "Prefix"
         | Branches _ -> "Branches"
         | Btree _ -> "Btree"
         | Leaf _ -> "Leaf"))
    prefixes
    (hist_int (fun (p, _) -> String.length p))
    prefixes;
  let btrees =
    List.filter_map
      (function
        | Btree { leaf; labels; branches } -> Some (leaf, labels, branches)
        | _ -> None)
      nodes
  in
  Format.fprintf ppf
    "@[<v 2>Btree nodes: %d@ With leaf: %d@ With size:@ %a@]@\n"
    (List.length btrees)
    (count (fun (l, _, _) -> Option.is_some l) btrees)
    (hist_int (fun (_, _, b) -> Array.length b))
    btrees

module Complete_tree = Complete_tree
