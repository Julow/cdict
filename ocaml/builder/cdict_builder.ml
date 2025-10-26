open Constants

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
    | Branches of 'a branches
    | Btree of string * Id.t array  (** Labels encodes a binary tree. *)
    | Prefix of string * Id.t  (** 1 to 3 bytes prefix *)
    | With_leaf of 'a * Id.t

  type 'a t = 'a node IdMap.t

  let add ?(node_id = Id.fresh ()) ids node =
    (IdMap.add node_id node ids, node_id)

  let find = IdMap.find

  let rec branches_to_list b =
    b.b_branches :: Option.fold ~none:[] ~some:branches_to_list b.b_next

  let rec fold_prefix prefix trie =
    if String.length prefix >= C.c_PREFIX_NODE_LENGTH then (prefix, trie)
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
      S.branches_t + 12
    in
    let branch_cost = S.ptr_t in
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
  let maybe_branches_to_btree b =
    let b = branches_to_list b in
    let n_branches = List.length b in
    let b = List.concat b in
    let len = List.length b in
    let has_nul_label = match b with ('\000', _) :: _ -> true | _ -> false in
    (* Btree nodes can't have more than [BTREE_NODE_LENGTH] branches and
       can't represent a NUL label. Don't make a Btree node if the branches
       node has few 'next' nodes. *)
    if
      len > C.c_BTREE_NODE_LENGTH
      || n_branches < branches_btree_cutoff
      || has_nul_label
    then None
    else
      let tree = Complete_tree.(to_array (of_sorted_list b)) in
      let labels =
        String.init C.c_BTREE_NODE_LENGTH (fun i ->
            if i < Array.length tree then fst tree.(i) else '\000')
      in
      let branches = Array.map snd tree in
      Some (Btree (labels, branches))

  (** Take the id as argument for making sure that the root node has id [0] and
      that branch nodes have an id smaller than their children. *)
  let rec node_of_trie ?node_id ids = function
    | { Trie.leaf = Some leaf; branches = [] } -> add ?node_id ids (Leaf leaf)
    | { leaf = Some leaf; branches } ->
        let ids, next = node_of_branches ids branches in
        add ?node_id ids (With_leaf (leaf, next))
    | { leaf = None; branches } -> node_of_branches ?node_id ids branches

  and node_of_branches ?node_id ids = function
    | [ (c, next) ] ->
        let prefix, next = fold_prefix (String.make 1 c) next in
        let ids, next = node_of_trie ids next in
        add ?node_id ids (Prefix (prefix, next))
    | branches ->
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
          match maybe_branches_to_btree branches with
          | Some n -> n
          | None -> Branches branches
        in
        add ?node_id ids node

  let of_trie trie = fst (node_of_trie ~node_id:Id.zero IdMap.empty trie)
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
    let w_int32 b node_off off i = Bytes.set_int32_le b.b (node_off + off) i
    let w_int8 b node_off off i = Bytes.set_uint8 b.b (node_off + off) i
    let w_bzero b node_off off len = Bytes.fill b.b (node_off + off) len '\000'

    let w_str b node_off off s =
      Bytes.blit_string s 0 b.b (node_off + off) (String.length s)
  end

  include Open
end

module Ptr = struct
  type kind = [ `Leaf | `Prefix | `Branches | `Btree | `With_leaf ]
  type t = Ptr of kind * int32

  let v kind ptr = Ptr (kind, Int32.of_int ptr)
  let v_leaf v = Ptr (`Leaf, Int32.(shift_left (of_int v) 3))
  (* let kind (Ptr (k, _)) = k *)
  (* let offset (Ptr (_, p)) = Int32.to_int p *)
  (* let with_kind kind (Ptr (_, p)) = Ptr (kind, p) *)
  (* let tag (Ptr (kind, _)) = Int32.to_int (tag_of_kind kind) *)

  let tag_of_kind = function
    | `Leaf -> C.tag_LEAF
    | `Prefix -> C.tag_PREFIX
    | `Branches -> C.tag_BRANCHES
    | `Btree -> C.tag_BTREE
    | `With_leaf -> C.tag_WITH_LEAF

  let to_int32 (Ptr (kind, ptr)) =
    if not Int32.(logand ptr C.c_PTR_KIND_MASK = 0l) then
      failwith "Writing misaligned pointer";
    Int32.(logor (tag_of_kind kind) ptr)

  let w b node_off off t = Buf.w_int32 b node_off off (to_int32 t)
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
    | Branches branches ->
        write_branches_node ?align b ~alloc_leaf nodes branches
    | Btree (labels, branches) ->
        write_btree_node ?align b ~alloc_leaf nodes labels branches
    | With_leaf (leaf, next) ->
        write_with_leaf ?align b ~alloc_leaf nodes leaf next

  and write_with_leaf ?align b ~alloc_leaf nodes leaf next =
    let off = alloc ?align b S.with_leaf_t in
    let next_ptr = write_node b ~alloc_leaf nodes next in
    let leaf_ptr = alloc_leaf leaf in
    Ptr.w b off O.with_leaf_t_leaf leaf_ptr;
    Ptr.w b off O.with_leaf_t_next next_ptr;
    Ptr.v `With_leaf off

  and write_prefix_node ?align b ~alloc_leaf nodes p next =
    let off = alloc ?align b S.prefix_t in
    let next_ptr = write_node b ~alloc_leaf nodes next in
    assert (String.length p <= C.c_PREFIX_NODE_LENGTH);
    w_bzero b off O.prefix_t_prefix C.c_PREFIX_NODE_LENGTH;
    for i = 0 to String.length p - 1 do
      w_int8 b off (O.prefix_t_prefix + i) (Char.code p.[i])
    done;
    Ptr.w b off O.prefix_t_next next_ptr;
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
    let off = alloc ?align b (S.branches_t + (len * S.ptr_t)) in
    let branch_off n = O.branches_t_branches + (n * S.ptr_t) in
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
        Ptr.w b off
          (branch_off (c - min_value))
          (write_node b ~alloc_leaf nodes id))
      brs.b_branches;
    w_int8 b off O.branches_t_low min_value;
    w_int8 b off O.branches_t_length len;
    w_int8 b off O.branches_t_has_next has_next;
    Ptr.v `Branches off

  and write_btree_node ?align b ~alloc_leaf nodes labels brs =
    let off = alloc ?align b (S.btree_t + (Array.length brs * S.ptr_t)) in
    let branch_off i = S.btree_t + (i * S.ptr_t) in
    w_bzero b off O.btree_t_labels C.c_BTREE_NODE_LENGTH;
    w_str b off O.btree_t_labels labels;
    Array.iteri
      (fun i n -> Ptr.w b off (branch_off i) (write_node b ~alloc_leaf nodes n))
      brs;
    Ptr.v `Btree off

  let write_tree b ~encode_leaf nodes =
    let alloc_leaf leaf = Ptr.v_leaf (encode_leaf leaf) in
    let header_off = alloc b S.header_t in
    assert (header_off = 0);
    let root_ptr = write_node b ~alloc_leaf nodes Optimized.Id.zero in
    Ptr.w b header_off O.header_t_root_ptr root_ptr
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
  let str_of_node_kind = function
    | Prefix _ -> "Prefix"
    | Branches _ -> "Branches"
    | Btree _ -> "Btree"
    | Leaf _ -> "Leaf"
    | With_leaf _ -> "With_leaf"
  in
  let count f lst =
    List.fold_left (fun n e -> if f e then n + 1 else n) 0 lst
  in
  let nodes = IdMap.fold (fun _ n acc -> n :: acc) tree [] in
  Format.fprintf ppf "Nodes: %d@\nLeaf nodes: %d@\n" (List.length nodes)
    (count (function Leaf _ -> true | _ -> false) nodes);
  let branches =
    List.filter_map (function Branches b -> Some b | _ -> None) nodes
  in
  let ranges = List.concat_map Optimized.branches_to_list branches in
  Format.fprintf ppf
    "@[<v 2>Branch nodes: %d@ With 'next' nodes:@ %a@ @[<v 2>Ranges: %d:@ \
     %a@]@]@\n"
    (List.length branches)
    (hist_int (fun b -> List.length (Optimized.branches_to_list b)))
    branches (List.length ranges) (hist_int List.length) ranges;
  let prefixes =
    List.filter_map
      (function Prefix (p, id) -> Some (p, id) | _ -> None)
      nodes
  in
  Format.fprintf ppf
    "@[<v 2>Prefix nodes: %d@ Followed by:@ %a@ With size:@ %a@]@\n"
    (List.length prefixes)
    (hist_str (fun (_, next) -> str_of_node_kind (IdMap.find next tree)))
    prefixes
    (hist_int (fun (p, _) -> String.length p))
    prefixes;
  let btrees =
    List.filter_map
      (function
        | Btree (labels, branches) -> Some (labels, branches) | _ -> None)
      nodes
  in
  Format.fprintf ppf "@[<v 2>Btree nodes: %d@ With size:@ %a@]@\n"
    (List.length btrees)
    (hist_int (fun (_, b) -> Array.length b))
    btrees;
  let with_leafs =
    List.filter_map
      (function With_leaf (_, next) -> Some next | _ -> None)
      nodes
  in
  Format.fprintf ppf "@[<v 2>With_leaf nodes: %d@ With next:@ %a@]@\n"
    (List.length with_leafs)
    (hist_str (fun next -> str_of_node_kind (IdMap.find next tree)))
    with_leafs;
  ()

let rec pp pp_leaf nodes ppf id =
  let open Optimized in
  let fpf fmt = Format.fprintf ppf fmt in
  match IdMap.find id nodes with
  | Leaf leaf -> fpf "@[<hv 2>Leaf@ %a@]" pp_leaf leaf
  | Branches brs ->
      let rec loop _ brs =
        List.iter
          (fun (c, id) -> fpf "%c %a@ " c (pp pp_leaf nodes) id)
          brs.b_branches;
        match brs.b_next with Some b -> fpf "next@ %a" loop b | None -> ()
      in
      fpf "@[<v 2>Branches@ %a@]" loop brs
  | Btree (labels, brs) ->
      fpf "@[<v 2>Btree@ ";
      for i = 0 to Array.length brs - 1 do
        fpf "%c %a@ " labels.[i] (pp pp_leaf nodes) brs.(i)
      done;
      fpf "@]"
  | Prefix (prefix, next) ->
      fpf "@[<v 2>Prefix %S@ %a@]" prefix (pp pp_leaf nodes) next
  | With_leaf (leaf, next) ->
      fpf "@[<v 2>With_leaf@ %a@ %a@]" pp_leaf leaf (pp pp_leaf nodes) next

let pp pp_leaf ppf nodes =
  pp pp_leaf nodes ppf Optimized.Id.zero;
  Format.fprintf ppf "@\n"

module Complete_tree = Complete_tree
