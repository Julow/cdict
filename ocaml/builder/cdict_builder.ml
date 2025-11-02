open Constants

module Optimized = struct
  (** Encode a trie into a form closer to libcdict's format, taking advantages
      of possible optimisations. *)

  module Id : sig
    type t = private int

    val fresh : unit -> t
    val compare : t -> t -> int
  end = struct
    type t = int

    let _uniq = ref 0

    let fresh () =
      incr _uniq;
      !_uniq (* Starts at 1. *)

    let compare = Int.compare
  end

  module IdMap = Map.Make (Id)

  type tr = { number : int; final : bool; next : Id.t }

  type branches = {
    b_next : branches option;  (** Sorted *)
    b_branches : (char * tr) list;  (** Sorted *)
  }

  type node =
    | Branches of branches
    | Btree of string * tr array  (** Labels encodes a binary tree. *)
    | Prefix of string * tr  (** Up to [C.c_PREFIX_NODE_LENGTH] bytes prefix *)
    | Number of int * tr

  type t = node IdMap.t * Id.t
  (** Second argument is the root id. *)

  let add ids node =
    let id = Id.fresh () in
    (IdMap.add id node ids, id)

  let find = IdMap.find

  let rec branches_to_list b =
    b.b_branches :: Option.fold ~none:[] ~some:branches_to_list b.b_next

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
      Some (labels, branches)

  module Seen = Dfa.Id.Map
  open Dfa

  let rec fold_prefix dfa prefix next number final =
    if String.length prefix >= C.c_PREFIX_NODE_LENGTH || number > 0 || final
    then (prefix, next, number, final)
    else
      match state dfa next with
      | [ { c = '\x00'; _ } ] | [] | _ :: _ :: _ -> (prefix, next, number, final)
      | [ { c; next; number = n; final } ] ->
          let prefix = prefix ^ String.make 1 c in
          fold_prefix dfa prefix next (number + n) final

  let rec node_of_dfa seen ids dfa sti =
    match Seen.find_opt sti !seen with
    | Some id -> (ids, id)
    | None ->
        let ids, id = node_of_dfa_uncached seen ids dfa (Dfa.state dfa sti) in
        seen := Seen.add sti id !seen;
        (ids, id)

  and node_of_tr seen ids dfa ~number ~final next =
    if number > C.c_PTR_NUMBER_MAX then
      let ids, next = node_of_tr seen ids dfa ~number:0 ~final next in
      let ids, next = add ids (Number (number, next)) in
      (ids, { next; number = 0; final = false })
    else
      let ids, next = node_of_dfa seen ids dfa next in
      (ids, { next; number; final })

  and node_of_dfa_uncached seen ids dfa = function
    | [ { c; next; number; final } ] ->
        let prefix, next, number, final =
          fold_prefix dfa (String.make 1 c) next number final
        in
        let ids, tr = node_of_tr seen ids dfa ~number ~final next in
        add ids (Prefix (prefix, tr))
    | trs ->
        let ids, branches =
          List.fold_right
            (fun { c; next; number; final } (ids, acc) ->
              let ids, tr = node_of_tr seen ids dfa ~number ~final next in
              (ids, (c, tr) :: acc))
            trs (ids, [])
        in
        assert (branches = List.sort compare branches);
        let branches = optimise_branches [] branches in
        let node =
          match maybe_branches_to_btree branches with
          | Some (cs, branches) -> Btree (cs, branches)
          | None -> Branches branches
        in
        add ids node

  let of_dfa dfa =
    let seen = ref Seen.empty in
    node_of_dfa_uncached seen IdMap.empty dfa (Dfa.root_state dfa)
end

module Freq : sig
  (** Encode an array of frequency into a 4 bits integer array using linear
      interpolation, losing precision. *)

  type t = private string

  val of_int_array : int array -> t

  val size : t -> int
  (** Size in bytes *)

  val get : t -> int -> int
  (** Get the frequency at the specified index. *)

  val to_int_list : t -> int list
  (** Append an extra [0] at the end if the number of frequency was originally
      odd. *)
end = struct
  type t = string

  let of_int_array_raw freq =
    let len = Array.length freq in
    let s = Bytes.create ((len + 1) / 2) in
    for f_i = 0 to len - 1 do
      let s_i = f_i / 2 in
      let f =
        let f = freq.(f_i) land 0xF in
        if f_i land 1 = 0 then f else (f lsl 4) lor Bytes.get_uint8 s s_i
      in
      Bytes.set_uint8 s s_i f
    done;
    Bytes.unsafe_to_string s

  let of_int_array freq =
    let freq_compressed =
      (* Compute the 0x10 medians and replace every frequencies by the cluster
         index they are assigned to. This compresses frequencies to a 4-bits
         number by loosing information. *)
      K_medians.k_medians freq 0x10 ~compare:Int.compare ~renumber:(fun _ c ->
          c)
    in
    of_int_array_raw freq_compressed

  let size = String.length

  let get t i =
    let c = Char.code t.[i / 2] in
    let c = if i land 1 = 0 then c else c lsr 4 in
    c land 0xF

  let to_int_list t = List.init (size t * 2) (get t)
end

type 'a t = Optimized.t * Freq.t

let of_list ~freq words =
  let words = Array.of_list words in
  Array.sort (fun (a, _) (b, _) -> String.compare a b) words;
  let dfa =
    Dfa.of_sorted_iter (fun f -> Array.iter (fun (w, _) -> f w) words)
  in
  let freq = Freq.of_int_array (Array.map (fun (_, data) -> freq data) words) in
  (Optimized.of_dfa dfa, freq)

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

type kind = [ `Prefix | `Branches | `Btree | `Number ]

module Ptr : sig
  type t

  val v : final:bool -> number:int -> kind -> int -> t
  val w : Buf.t -> int -> int -> t -> unit
end = struct
  type t = int32

  let tag_of_kind = function
    | `Prefix -> C.tag_PREFIX
    | `Branches -> C.tag_BRANCHES
    | `Btree -> C.tag_BTREE
    | `Number -> C.tag_NUMBER

  let v ~final ~number kind offset =
    let open Int32 in
    let final_flag = if final then C.flag_PTR_FLAG_FINAL else 0l in
    assert (number land lnot C.c_PTR_NUMBER_MAX = 0);
    let number = shift_left (of_int number) C.c_PTR_NUMBER_OFFSET in
    let offset = of_int offset in
    assert (logand number (lognot C.mask_PTR_NUMBER_MASK) = 0l);
    assert (logand offset (lognot C.mask_PTR_OFFSET_MASK) = 0l);
    logor number (logor final_flag (logor (tag_of_kind kind) offset))

  let w b node_off off ptr = Buf.w_int32 b node_off off ptr
end

module Writer = struct
  open Buf.Open

  let[@inline] align8 offset =
    let down = offset land lnot 7 in
    if down = offset then down else down + 8

  let[@inline] align4 offset =
    let down = offset land lnot 3 in
    if down = offset then down else down + 4

  module Seen = Optimized.IdMap

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

  let rec write_node seen ?align b nodes { Optimized.final; number; next } =
    write_node' seen ?align b nodes ~final ~number next

  and write_node' seen ?align b nodes ~final ~number next =
    let kind, offset =
      match Seen.find_opt next !seen with
      | Some node -> node
      | None ->
          let node =
            match Optimized.find next nodes with
            (* | exception Not_found -> . *)
            | Optimized.Prefix (p, tr) ->
                (`Prefix, write_prefix_node seen ?align b nodes p tr)
            | Branches branches ->
                (`Branches, write_branches_node seen ?align b nodes branches)
            | Btree (labels, branches) ->
                (`Btree, write_btree_node seen ?align b nodes labels branches)
            | Number (n, next) ->
                (`Number, write_number_node seen ?align b nodes n next)
          in
          seen := Seen.add next node !seen;
          node
    in
    Ptr.v ~final ~number kind offset

  and write_number_node seen ?align b nodes n next =
    let off = alloc ?align b S.number_t in
    let next = write_node seen b nodes next in
    w_int32 b off O.number_t_number (Int32.of_int n);
    Ptr.w b off O.number_t_next next;
    off

  and write_prefix_node seen ?align b nodes p next =
    let off = alloc ?align b S.prefix_t in
    let next_ptr = write_node seen b nodes next in
    assert (String.length p <= C.c_PREFIX_NODE_LENGTH);
    w_bzero b off O.prefix_t_prefix C.c_PREFIX_NODE_LENGTH;
    for i = 0 to String.length p - 1 do
      w_int8 b off (O.prefix_t_prefix + i) (Char.code p.[i])
    done;
    Ptr.w b off O.prefix_t_next next_ptr;
    off

  and write_branches_node seen ?align b nodes brs =
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
          ignore (write_branches_node seen ~align:false b nodes next);
          1
      | None -> 0
    in
    List.iter
      (fun (c, id) ->
        let c = Char.code c in
        Ptr.w b off (branch_off (c - min_value)) (write_node seen b nodes id))
      brs.b_branches;
    w_int8 b off O.branches_t_low min_value;
    w_int8 b off O.branches_t_length len;
    w_int8 b off O.branches_t_has_next has_next;
    off

  and write_btree_node seen ?align b nodes labels brs =
    let off = alloc ?align b (S.btree_t + (Array.length brs * S.ptr_t)) in
    let branch_off i = S.btree_t + (i * S.ptr_t) in
    w_bzero b off O.btree_t_labels C.c_BTREE_NODE_LENGTH;
    w_str b off O.btree_t_labels labels;
    Array.iteri
      (fun i n -> Ptr.w b off (branch_off i) (write_node seen b nodes n))
      brs;
    off

  let write_tree b ((nodes, root_id), freq) =
    let seen = ref Seen.empty in
    let header_off = alloc b S.header_t in
    assert (header_off = 0);
    let root_tr = { Optimized.final = false; number = 0; next = root_id } in
    let root_ptr = write_node seen b nodes root_tr in
    Ptr.w b header_off O.header_t_root_ptr root_ptr;
    let freq_off = alloc b (Freq.size freq) in
    w_int32 b header_off O.header_t_freq_off (Int32.of_int freq_off);
    w_str b freq_off 0 (freq :> string)
end

let to_buf tree =
  let b = Buf.create 1_000_000 in
  Writer.write_tree b tree;
  b

let to_string tree = Buf.to_string (to_buf tree)
let output tree out_chan = Buf.output out_chan (to_buf tree)

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

let stats ppf ((tree, _root_id), freq) =
  let open Optimized in
  let str_of_node_kind = function
    | Prefix _ -> "Prefix"
    | Branches _ -> "Branches"
    | Btree _ -> "Btree"
    | Number _ -> "Number"
  in
  let str_of_node_kind' tr = str_of_node_kind (IdMap.find tr.next tree) in
  let tr_is_final tr = if tr.final then "Final" else "Non-final" in
  let pp_transitions ppf trs =
    let tr_num_bytes { number = n; _ } =
      if n <= 0xFFFF then if n <= 0xFF then 1 else 2
      else if n <= 0xFFFFFF then 3
      else 4
    in
    Format.fprintf ppf
      "@[<v 2>Transitions: %d@ %a@ With numbers length in bytes: %a@]"
      (List.length trs) (hist_str tr_is_final) trs (hist_int tr_num_bytes) trs
  in
  let nodes = IdMap.fold (fun _ n acc -> n :: acc) tree [] in
  Format.fprintf ppf "Nodes: %d@\n" (List.length nodes);
  let branches =
    List.filter_map (function Branches b -> Some b | _ -> None) nodes
  in
  let ranges = List.concat_map Optimized.branches_to_list branches in
  Format.fprintf ppf
    "@[<v 2>Branch nodes: %d@ With 'next' nodes:@ %a@ @[<v 2>Ranges: %d:@ \
     %a@]@ %a@]@\n"
    (List.length branches)
    (hist_int (fun b -> List.length (Optimized.branches_to_list b)))
    branches (List.length ranges) (hist_int List.length) ranges pp_transitions
    (List.concat_map (List.map snd) ranges);
  let prefixes =
    List.filter_map
      (function Prefix (p, id) -> Some (p, id) | _ -> None)
      nodes
  in
  let prefixes_trs = List.map snd prefixes in
  Format.fprintf ppf
    "@[<v 2>Prefix nodes: %d@ Followed by:@ %a@ With size:@ %a@ %a@]@\n"
    (List.length prefixes)
    (hist_str str_of_node_kind')
    prefixes_trs
    (hist_int (fun (p, _) -> String.length p))
    prefixes pp_transitions prefixes_trs;
  let btrees =
    List.filter_map
      (function
        | Btree (labels, branches) -> Some (labels, branches) | _ -> None)
      nodes
  in
  let btrees_ranges = List.map snd btrees in
  let btrees_trs = List.concat_map Array.to_list btrees_ranges in
  Format.fprintf ppf "@[<v 2>Btree nodes: %d@ With size:@ %a@ %a@]@\n"
    (List.length btrees) (hist_int Array.length) btrees_ranges pp_transitions
    btrees_trs;
  let numbers =
    List.filter_map (function Number (_, next) -> Some next | _ -> None) nodes
  in
  Format.fprintf ppf
    "@[<v 2>Number nodes: %d@ With next:@ %a@ With final: %a@]@\n"
    (List.length numbers)
    (hist_str str_of_node_kind')
    numbers (hist_str tr_is_final) numbers;
  let freq = Freq.to_int_list freq in
  Format.fprintf ppf "@[<v 2>Freq: %d@ With value:@ %a@]@\n" (List.length freq)
    (hist_int Fun.id) freq;
  ()

let rec pp freq nodes index ppf id =
  let open Optimized in
  let fpf fmt = Format.fprintf ppf fmt in
  match IdMap.find id nodes with
  | Branches brs ->
      let rec loop _ brs =
        List.iter
          (fun (c, tr) -> fpf "%c %a@ " c (pp_tr freq nodes index) tr)
          brs.b_branches;
        match brs.b_next with Some b -> fpf "next@ %a" loop b | None -> ()
      in
      fpf "@[<v 2>Branches@ %a@]" loop brs
  | Btree (labels, brs) ->
      fpf "@[<v 2>Btree@ ";
      for i = 0 to Array.length brs - 1 do
        fpf "%c %a@ " labels.[i] (pp_tr freq nodes index) brs.(i)
      done;
      fpf "@]"
  | Prefix (prefix, next) ->
      fpf "@[<v 2>Prefix %S@ %a@]" prefix (pp_tr freq nodes index) next
  | Number (_, next) -> fpf "@[<v 2>Number@ %a@]" (pp_tr freq nodes index) next

and pp_tr freq nodes index ppf tr =
  let index = index + tr.number + if tr.final then 1 else 0 in
  Format.fprintf ppf "freq=%d@ " (Freq.get freq index);
  pp freq nodes index ppf tr.next

let pp ppf ((nodes, root_id), freq) = pp freq nodes 0 ppf root_id

module Complete_tree = Complete_tree
module K_medians = K_medians
