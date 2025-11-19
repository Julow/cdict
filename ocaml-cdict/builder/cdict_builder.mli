(** This module allows writing dictionaries for the cdict C library. *)

type 'a t
(** A collection of words. Words can be arbitrary strings with arbitrary
    metadata attached. This stores the entire dictionary in memory, in a
    space-inefficient way. *)

val of_list : freq:('a -> int) -> (string * 'a) list -> 'a t
(** Construct a dictionary from a list. *)

val output : 'a t -> Out_channel.t -> unit

val to_string : 'a t -> string
(** Like [output] but write into a string in memory. *)

val stats : Format.formatter -> 'a t -> unit
(** Print various stats for debugging and testing purposes. *)

val pp : Format.formatter -> 'a t -> unit

(**/**)

(** Exposed for testing purposes *)

module Complete_tree : sig
  type 'a t

  val of_sorted_list : 'a list -> 'a t
  val to_array : 'a t -> 'a array
end

module K_medians : sig
  val k_medians :
    'a array ->
    int ->
    compare:('a -> 'a -> int) ->
    renumber:('a -> int -> 'b) ->
    'b array
end

module Sized_int_array : module type of Sized_int_array
