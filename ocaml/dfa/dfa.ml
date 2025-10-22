module IntMap = Map.Make (Int)

module Id : sig
  type t = private int

  val zero : t

  val fresh : unit -> t
  (** Returns a unique ID. *)

  val compare : t -> t -> int

  module Map : Map.S with type key = t
end = struct
  type t = int

  let compare = Int.compare
  let zero = 0
  let _uniq = ref 0

  let fresh () =
    incr _uniq;
    !_uniq (* Starts at 1. *)

  module Map = IntMap
end

module M = Id.Map

type id = Id.t

type 'a state = {
  tr : (char * id) list;  (** Sorted *)
  leaves : 'a list;  (** Reverse order of the input *)
}

type 'a t = 'a state M.t

let state m id = M.find id m
let root_state m = state m Id.zero
let add m id st = M.add id st m

let new_state m ~leaves tr =
  let id = Id.fresh () in
  (add m id { tr; leaves }, id)

let str_separate s index =
  (String.sub s 0 index, String.sub s index (String.length s - index))

let list_last = function
  | [] -> None
  | hd :: tl -> Some (List.fold_left (fun _ e -> e) hd tl)

let pp pp_metadata ppf m =
  let module S = Set.Make (Id) in
  let fpf = Format.fprintf in
  let seen = ref S.empty in
  let rec pp_st ppf sti =
    match M.find_opt sti m with
    | Some _ when S.mem sti !seen -> fpf ppf " (%d seen)" (sti :> int)
    | Some st ->
        seen := S.add sti !seen;
        fpf ppf " %-4d@[<v>" (sti :> int);
        (match st.leaves with
        | [] -> ()
        | leaves ->
            fpf ppf "<@[<hov>final %a>@]"
              Format.(pp_print_list ~pp_sep:pp_print_space pp_metadata)
              leaves;
            if st.tr <> [] then fpf ppf "@ ");
        fpf ppf "%a@]" Format.(pp_print_list ~pp_sep:pp_print_space pp_tr) st.tr
    | None -> fpf ppf " <removed>"
  and pp_tr ppf (c, st) = fpf ppf "%C%a" c pp_st st in
  fpf ppf ".%a" pp_st Id.zero

module State = struct
  type t = S : 'a state -> t [@@unboxed]

  (** Compare two states structurally. Leaves are not compared. *)
  let compare (S a) (S b) = compare a.tr b.tr
end

(** From the paper https://arxiv.org/pdf/cs/0007009 with some differences:

    - Final nodes store metadata of the words that they represent.
      Word metadata are not associated to words but to final nodes that were
      there in the previous algorithm. This means that some final nodes will
      contain several word metadata with no way to disambiguate.
    - Final and non-final nodes with equal transitions are considered equal.
      Word metadata should allow disambiguating whether the end of a word is
      reached.
    - Word metadata are merged in [replace_or_register] when the last children
      is replaced by an equivalent node. *)

module Register = Map.Make (State)

let common_prefix m word =
  let rec loop id i =
    if i >= String.length word then (i, id)
    else
      let st = state m id in
      match List.assoc_opt word.[i] st.tr with
      | Some next -> loop next (i + 1)
      | None -> (i, id)
  in
  loop Id.zero 0

let last_child st =
  match list_last st.tr with None -> assert false | Some (_, next) -> next

let with_last_child st q =
  let rec set_last_tr = function
    | [] -> assert false
    | [ (c, _) ] -> [ (c, q) ]
    | hd :: tl -> hd :: set_last_tr tl
  in
  { st with tr = set_last_tr st.tr }

(** Assumes that no prefix of [suffix] is present in [st]. *)
let add_suffix m sti suffix leaf =
  let len = String.length suffix in
  let rec loop m i =
    let m, next_state_id =
      if i + 1 = len then new_state m ~leaves:[ leaf ] []
      else
        let m, tr' = loop m (i + 1) in
        new_state m ~leaves:[] [ tr' ]
    in
    (m, (suffix.[i], next_state_id))
  in
  let st = state m sti in
  if len = 0 then add m sti { st with leaves = leaf :: st.leaves }
  else
    let m, tr' = loop m 0 in
    add m sti { st with tr = st.tr @ [ tr' ] }

let has_children st = st.tr <> []

(** Find a state equivalent to [st] in [reg]. *)
let equivalent_state reg m st =
  let st = State.S st in
  match Register.find_opt st reg with
  | Some sti as r ->
      let st' = state m sti in
      (reg, if State.compare st (State.S st') = 0 then r else None)
  | None -> (reg, None)

let rec replace_or_register reg m st sti =
  let childi = last_child st in
  let reg, m =
    let child = state m childi in
    if has_children child then replace_or_register reg m child childi
    else (reg, m)
  in
  let child = state m childi in
  match equivalent_state reg m child with
  | reg, Some qi ->
      (* [child] is replaced by [q], merge leaves. *)
      let q = state m qi in
      let m = add m qi { q with leaves = List.rev_append child.leaves q.leaves } in
      let m = M.remove childi m in
      let m = add m sti (with_last_child st qi) in
      (Register.remove (State.S st) reg, m)
  | reg, None -> (Register.add (State.S child) childi reg, m)

let add_word_sorted (reg, m) (word, leaf) =
  let prefix_len, last_statei = common_prefix m word in
  let last_state = state m last_statei in
  let _, current_suffix = str_separate word prefix_len in
  let reg, m =
    if has_children last_state then
      replace_or_register reg m last_state last_statei
    else (reg, m)
  in
  (reg, add_suffix m last_statei current_suffix leaf)

let of_sorted_list words =
  let empty = (Register.empty, M.singleton Id.zero { tr = []; leaves = [] }) in
  let reg, m = List.fold_left add_word_sorted empty words in
  snd (replace_or_register reg m (state m Id.zero) Id.zero)
