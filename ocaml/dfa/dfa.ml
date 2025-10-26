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

type 'a transition = {
  c : char;
  next : id;
  leaves : 'a list;  (** Reverse order of the input *)
}

type 'a state = 'a transition list
(** Sorted by [c] *)

type 'a t = 'a state M.t

let state m id = M.find id m
let root_state m = state m Id.zero

let new_state m tr =
  let id = Id.fresh () in
  (M.add id tr m, id)

let str_separate s index =
  (String.sub s 0 index, String.sub s index (String.length s - index))

let rec list_last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: tl -> list_last tl

let pp pp_metadata ppf m =
  let module S = Set.Make (Id) in
  let fpf = Format.fprintf in
  let pplist pp_a = Format.(pp_print_list ~pp_sep:pp_print_space pp_a) in
  let seen = ref S.empty in
  let rec pp_st ppf sti =
    match M.find_opt sti m with
    | Some _ when S.mem sti !seen -> fpf ppf "(%d seen)" (sti :> int)
    | Some trs ->
        seen := S.add sti !seen;
        fpf ppf "%-4d@[<v>%a@]" (sti :> int) (pplist pp_tr) trs
    | None -> fpf ppf " <removed>"
  and pp_leaves ppf leaves =
    if leaves = [] then ()
    else fpf ppf "<leaf @[<hov>%a@]>@ " (pplist pp_metadata) leaves
  and pp_tr ppf tr =
    fpf ppf "%C @[<v>%a%a@]" tr.c pp_leaves tr.leaves pp_st tr.next
  in
  fpf ppf ".%a" pp_st Id.zero

module State = struct
  type t = S : 'a state -> t [@@unboxed]

  (** Compare two states structurally. Leaves are not compared. *)
  let compare (S a) (S b) =
    let rec loop a b =
      match (a, b) with
      | [], [] -> 0
      | [], _ -> ~-1
      | _, [] -> 1
      | a :: at, b :: bt ->
          let d = Char.compare a.c b.c in
          if d <> 0 then d
          else
            let d = Id.compare a.next b.next in
            if d <> 0 then d else loop at bt
    in
    loop a b
end

(** From the paper https://arxiv.org/pdf/cs/0007009 with some differences:

    - Final transitions are used instead of final nodes to represent the end of
      words.
    - Final and non-final transitions with the same character and [next] pointer
      are considered equal.
    - Transitions are merged and a single transition can hold several word
      metadata. The DFA contain no way to disambiguate between the word
      metadata.
    - Word metadata are merged in [replace_or_register] when the last children
      is replaced by an equivalent node. *)

module R = Map.Make (State)
(** Register *)

let common_prefix m word =
  let rec loop id i =
    if i >= String.length word then (i, id)
    else
      let st = state m id in
      let c = word.[i] in
      match List.find_opt (fun tr -> tr.c = c) st with
      | Some { next; _ } -> loop next (i + 1)
      | None -> (i, id)
  in
  loop Id.zero 0

let rec with_last_child st q =
  match st with
  | [] -> assert false
  | [ tr ] -> [ { tr with next = q } ]
  | hd :: tl -> hd :: with_last_child tl q

(** Assumes that no prefix of [suffix] is present in [st]. *)
let add_suffix m sti suffix leaf =
  let len = String.length suffix in
  let rec loop m i =
    let (m, next), leaves =
      if i + 1 = len then (new_state m [], [ leaf ])
      else
        let m, tr' = loop m (i + 1) in
        (new_state m [ tr' ], [])
    in
    (m, { c = suffix.[i]; next; leaves })
  in
  if len = 0 then (* Remove a duplicate. *) m
  else
    let st = state m sti in
    let m, tr' = loop m 0 in
    M.add sti (st @ [ tr' ]) m

(** Merge the leaves of states with the same transitions. To ensure some order
    in the leaves, [a] is the state we are replacing and [b] is the equivalent
    state found in the register. *)
let merge_equivalent_states a b =
  List.map2
    (fun a b ->
      assert (a.c = b.c && a.next = b.next);
      { a with leaves = List.rev_append a.leaves b.leaves })
    a b

(** Find a state equivalent to [st] in [reg]. *)
let equivalent_state reg m st =
  let st_wrapped = State.S st in
  match R.find_opt st_wrapped reg with
  | Some sti ->
      let st' = state m sti in
      assert (State.compare st_wrapped (State.S st') = 0);
      Some (st', sti)
  | None -> None

let rec replace_or_register reg m sti =
  let st = state m sti in
  match list_last st with
  | None -> (* No children *) (reg, m)
  | Some { c = _; next = childi; leaves = _ } -> (
      let reg, m = replace_or_register reg m childi in
      let child = state m childi in
      match equivalent_state reg m child with
      | Some (q, qi) ->
          (* Merge word metadata and update [q]. *)
          let q = merge_equivalent_states child q in
          let m =
            M.remove childi m |> M.add qi q |> M.add sti (with_last_child st qi)
          and reg =
            R.remove (State.S st) reg |> R.remove (State.S child)
            |> R.add (State.S q) qi
          in
          (reg, m)
      | None -> (R.add (State.S child) childi reg, m))

let add_word_sorted (reg, m) (word, leaf) =
  let prefix_len, last_statei = common_prefix m word in
  let _, current_suffix = str_separate word prefix_len in
  let reg, m = replace_or_register reg m last_statei in
  (reg, add_suffix m last_statei current_suffix leaf)

let of_sorted_list words =
  let empty = (R.empty, M.singleton Id.zero []) in
  let reg, m = List.fold_left add_word_sorted empty words in
  snd (replace_or_register reg m Id.zero)
