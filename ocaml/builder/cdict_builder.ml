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
  type sized_integer_array = [ `I8 | `I16 ] * int array

  type branches = {
    labels : string;  (** Labels encodes a binary tree. *)
    branches : tr array;  (** Same length as [labels]. *)
    numbers : sized_integer_array option;  (** Same length as [labels]. *)
  }

  type node = Branches of branches | Prefix of string * tr

  type t = node IdMap.t * Id.t
  (** Second argument is the root id. *)

  let add ids node =
    let id = Id.fresh () in
    (IdMap.add id node ids, id)

  let find = IdMap.find

  let encode_branches brs =
    let length = List.length brs in
    let tree = Complete_tree.(to_array (of_sorted_list brs)) in
    let labels = String.init length (fun i -> fst tree.(i)) in
    let branches = Array.map snd tree in
    let branches, numbers =
      if Array.exists (fun tr -> tr.number > C.c_MAX_PTR_NUMBER) branches then
        let numbers =
          Array.map (fun tr -> tr.number / C.c_MAX_PTR_NUMBER) branches
        in
        let branches =
          Array.map
            (fun tr -> { tr with number = tr.number mod C.c_MAX_PTR_NUMBER })
            branches
        in
        let n_size =
          if Array.exists (fun n -> n > 0xFF) numbers then `I16 else `I8
        in
        (branches, Some (n_size, numbers))
      else (branches, None)
    in
    Branches { labels; branches; numbers }

  module Seen = Dfa.Id.Map
  open Dfa

  let rec fold_prefix dfa prefix next final =
    (* The maximum prefix length is [C.c_MAX_PTR_NUMBER] because it is encoded
       in the number field. *)
    if final || String.length prefix >= C.c_MAX_PTR_NUMBER then
      (prefix, next, final)
    else
      match state dfa next with
      | [ { c; next; number = 0; final } ] ->
          let prefix = prefix ^ String.make 1 c in
          fold_prefix dfa prefix next final
      | _ -> (prefix, next, final)

  let rec node_of_dfa seen ids dfa sti =
    match Seen.find_opt sti !seen with
    | Some id -> (ids, id)
    | None ->
        let ids, id = node_of_dfa_uncached seen ids dfa (Dfa.state dfa sti) in
        seen := Seen.add sti id !seen;
        (ids, id)

  and node_of_dfa_uncached seen ids dfa = function
    | [ { c; next; number = 0; final } ] ->
        let prefix, next, final =
          fold_prefix dfa (String.make 1 c) next final
        in
        let ids, next = node_of_dfa seen ids dfa next in
        add ids (Prefix (prefix, { next; number = 0; final }))
    | trs ->
        let ids, branches =
          List.fold_right
            (fun { c; next; number; final } (ids, acc) ->
              let ids, next = node_of_dfa seen ids dfa next in
              (ids, (c, { next; number; final }) :: acc))
            trs (ids, [])
        in
        assert (branches = List.sort compare branches);
        let node = encode_branches branches in
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
    let w_uint8 b node_off off i = Bytes.set_uint8 b.b (node_off + off) i
    let w_uint16 b node_off off i = Bytes.set_uint16_le b.b (node_off + off) i
    let w_bzero b node_off off len = Bytes.fill b.b (node_off + off) len '\000'

    let w_str b node_off off s =
      Bytes.blit_string s 0 b.b (node_off + off) (String.length s)
  end

  include Open
end

type kind = [ `Prefix | `Branches ]

module Ptr : sig
  type t = private int32

  val v : final:bool -> number:int -> kind -> int -> t
  val w : Buf.t -> int -> int -> t -> unit
end = struct
  type t = int32

  let tag_of_kind = function
    | `Prefix -> C.tag_PREFIX
    | `Branches -> C.tag_BRANCHES

  let v ~final ~number kind offset =
    let open Int32 in
    let final_flag = if final then C.flag_PTR_FLAG_FINAL else 0l in
    assert (number land lnot C.c_MAX_PTR_NUMBER = 0);
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
            | Branches brs ->
                (`Branches, write_branches_node seen ?align b nodes brs)
          in
          seen := Seen.add next node !seen;
          node
    in
    Ptr.v ~final ~number kind offset

  and write_prefix_node seen ?align b nodes p next =
    let len = String.length p in
    let off = alloc ?align b (S.prefix_t + len) in
    let next_ptr :> int32 = write_node seen b nodes next in
    assert (Int32.logand next_ptr C.mask_PTR_NUMBER_MASK = 0l);
    let next_ptr =
      Int32.(logor next_ptr (shift_left (of_int len) C.c_PTR_NUMBER_OFFSET))
    in
    w_int32 b off O.prefix_t_next_ptr_and_len next_ptr;
    w_str b off O.prefix_t_prefix p;
    off

  and write_numbers w_number number_byte_size numbers b off =
    Array.iteri (fun i n -> w_number b off (i * number_byte_size) n) numbers

  and write_branches_node seen ?align b nodes { labels; branches; numbers } =
    let length = String.length labels in
    let branch_start_off = align4 (S.branches_t + length) in
    let numbers_start_off = branch_start_off + (length * S.ptr_t) in
    let number_byte_size, numbers_format, w_numbers =
      match numbers with
      | Some (`I8, n) -> (1, C.c_NUMBERS_8_BITS, write_numbers w_uint8 1 n)
      | Some (`I16, n) -> (2, C.c_NUMBERS_16_BITS, write_numbers w_uint16 2 n)
      | None -> (0, C.c_NUMBERS_NONE, fun _ _ -> ())
    in
    let off =
      alloc ?align b (numbers_start_off + (length * number_byte_size))
    in
    let branch_off i = branch_start_off + (i * S.ptr_t) in
    let header = numbers_format in
    (* Ensure the padding is zero'd *)
    w_bzero b off 0 branch_start_off;
    w_uint8 b off O.branches_t_header header;
    w_uint8 b off O.branches_t_length length;
    w_str b off O.branches_t_labels labels;
    Array.iteri
      (fun i n -> Ptr.w b off (branch_off i) (write_node seen b nodes n))
      branches;
    w_numbers b (off + numbers_start_off);
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
  Format.fprintf ppf
    "@[<v 2>Branch nodes: %d@ With size:@ %a@ %a@ With numbers: %a@]@\n"
    (List.length branches)
    (hist_int (fun b -> String.length b.labels))
    branches pp_transitions
    (List.concat_map (fun b -> Array.to_list b.branches) branches)
    (hist_str (fun b ->
         match b.numbers with
         | Some (`I8, _) -> "8 bits"
         | Some (`I16, _) -> "16 bits"
         | None -> "None"))
    branches;
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
  let freq = Freq.to_int_list freq in
  Format.fprintf ppf "@[<v 2>Freq: %d@ With value:@ %a@]@\n" (List.length freq)
    (hist_int Fun.id) freq;
  ()

let rec pp freq nodes index ppf id =
  let open Optimized in
  let fpf fmt = Format.fprintf ppf fmt in
  match IdMap.find id nodes with
  | Branches { labels; branches; _ } ->
      fpf "@[<v 2>Branches@ ";
      for i = 0 to Array.length branches - 1 do
        fpf "%c %a@ " labels.[i] (pp_tr freq nodes index) branches.(i)
      done;
      fpf "@]"
  | Prefix (prefix, next) ->
      fpf "@[<v 2>Prefix %S@ %a@]" prefix (pp_tr freq nodes index) next

and pp_tr freq nodes index ppf tr =
  let index = index + tr.number + if tr.final then 1 else 0 in
  Format.fprintf ppf "freq=%d@ " (Freq.get freq index);
  pp freq nodes index ppf tr.next

let pp ppf ((nodes, root_id), freq) = pp freq nodes 0 ppf root_id

module Complete_tree = Complete_tree
module K_medians = K_medians
