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

type transition = {
  c : char;
  next : id;
  number : int;
      (** Equal to [-1] during construction, computed in a second step. *)
  final : bool;
}

type state = transition list
(** Sorted by [c] *)

type t = state M.t

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

let pp ppf m =
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
  and pp_tr ppf tr =
    fpf ppf "%C @[<v>(n=%d)" tr.c tr.number;
    if tr.final then fpf ppf " (final)";
    fpf ppf "@ %a@]" pp_st tr.next
  in
  fpf ppf ".%a" pp_st Id.zero

module State = struct
  type t = state

  (** Compare two states structurally. The [index] field is not compared. *)
  let compare a b =
    List.compare
      (fun a b ->
        let d = Char.compare a.c b.c in
        if d <> 0 then d
        else
          let d = Id.compare a.next b.next in
          if d <> 0 then d else Bool.compare a.final b.final)
      a b
end

(** The construction algorithm is from the paper:
    {v
    Incremental Construction of Minimal Acyclic Finite-State Automata
    by Jan Daciuk, Stoyan Mihov, Bruce Watson, Richard Watson
    https://arxiv.org/pdf/cs/0007009
    v}
    With the following differences:

    - Final transitions are used instead of final nodes to represent the end of
      words.
    - Duplicated words in the input list do not crash the program.
    - Additional bookkeeping is added to reduce the size of the register during
      construction.
    - The incremental algorithm is not used.

    The perfect hashing scheme is from the paper:
    {v
    Applications of Finite Automata Representing Large Vocabularies
    by Cláudio L. Lucchesi and Tomasz Kowaltowski
    https://www.cs.mun.ca/~harold/Courses/Old/CS4750.F14/Diary/1992-001.pdf
    v}
    With the following differences:

    - Only the 'number' field from the traversed transitions need to be read.
      The preceding transitions in the traversed nodes are no longer taken into
      account. *)

module R = Map.Make (State)
(** Register *)

let common_prefix m word =
  let rec loop id i =
    if i >= String.length word then (i, id)
    else
      let st = M.find id m in
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
let add_suffix m sti suffix =
  let len = String.length suffix in
  let rec loop m i =
    let (m, next), final =
      if i + 1 = len then (new_state m [], true)
      else
        let m, tr' = loop m (i + 1) in
        (new_state m [ tr' ], false)
    in
    (m, { c = suffix.[i]; next; number = ~-1; final })
  in
  if len = 0 then (* Remove a duplicate. *) m
  else
    let st = M.find sti m in
    let m, tr' = loop m 0 in
    M.add sti (st @ [ tr' ]) m

(** Find a state equivalent to [st] in [reg]. *)
let equivalent_state reg m st =
  match R.find_opt st reg with
  | Some sti ->
      let st' = M.find sti m in
      assert (State.compare st st' = 0);
      Some (st', sti)
  | None -> None

let rec replace_or_register reg m sti =
  let st = M.find sti m in
  match list_last st with
  | None -> (* No children *) (reg, m)
  | Some { c = _; next = childi; _ } -> (
      let reg, m = replace_or_register reg m childi in
      let child = M.find childi m in
      match equivalent_state reg m child with
      | Some (q, qi) ->
          let m =
            m |> M.remove childi |> M.add qi q
            |> M.add sti (with_last_child st qi)
          and reg = reg |> R.remove st |> R.remove child |> R.add q qi in
          (reg, m)
      | None -> (R.add child childi reg, m))

let add_word_sorted (reg, m) word =
  let prefix_len, last_statei = common_prefix m word in
  let _, current_suffix = str_separate word prefix_len in
  let reg, m = replace_or_register reg m last_statei in
  (reg, add_suffix m last_statei current_suffix)

let numbers_state m =
  let rec map_trs m index acc = function
    | tr :: tl ->
        let acc = { tr with number = index } :: acc in
        let m, size = map_st m 0 tr.next in
        let size = if tr.final then size + 1 else size in
        map_trs m (index + size) acc tl
    | [] -> (m, index, List.rev acc)
  and map_st m index sti =
    let m, size, trs = map_trs m 0 [] (M.find sti m) in
    (M.add sti trs m, index + size)
  in
  fst (map_st m 0 Id.zero)

let of_sorted_list words =
  let empty = (R.empty, M.singleton Id.zero []) in
  let reg, m = List.fold_left add_word_sorted empty words in
  let _, m = replace_or_register reg m Id.zero in
  numbers_state m
