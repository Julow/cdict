(** This module allows writing dictionaries for the cdict C library. *)

type 'a t
(** A collection of words. Words can be arbitrary strings with arbitrary
    metadata attached. This stores the entire dictionary in memory, in a
    space-inefficient way. *)

val of_list : (string * 'a) list -> 'a t
(** Construct a dictionary from a list. *)

val output : 'a t -> encode_leaf:('a -> int) -> Out_channel.t -> unit
(** [encode_leaf] encode tree leaves into a 29-bits integer. *)

val to_string : 'a t -> encode_leaf:('a -> int) -> string
(** Like [output] but write into a string in memory. *)

val stats : Format.formatter -> 'a t -> unit
(** Print various stats for debugging and testing purposes. *)

(**/**)

(** Exposed for testing purposes *)

module Complete_tree : sig
  type 'a t

  val of_sorted_list : 'a list -> 'a t
  val to_array : 'a t -> 'a array
end
