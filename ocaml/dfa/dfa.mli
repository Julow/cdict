(** Implements a minimal deterministic finite-state acyclic automaton that
    recognizes a finite set of words. It works with a 8 bits alphabet and treat
    words as byte strings.

    It is based on the algorithm from the paper:
    {v
    Incremental Construction of Minimal Acyclic Finite-State Automata
    by Jan Daciuk, Stoyan Mihov, Bruce Watson, Richard Watson
    https://arxiv.org/pdf/cs/0007009
    v}
    The incremental algorithm is not used. *)

type id

type 'a transition = {
  c : char;
  next : id;
  leaves : 'a list;
      (** Metadata of words that ends at this state. A state is final
          if-and-only-if it has a non-empty [leafs] field. *)
}

type 'a state = 'a transition list
(** Transitions sorted in lexicographic order. *)

type 'a t

val state : 'a t -> id -> 'a state
(** Access a state. From its ID. IDs are found in a state [tr] field. *)

val root_state : 'a t -> 'a state
(** Access the root state. *)

val of_sorted_list : (string * 'a) list -> 'a t
(** Construct a minimal DFA from a lexicographically sorted list. If the list is
    not sorted, the DFA will not be minimal. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty print the internal structure of the DFA. For debugging purposes. *)
