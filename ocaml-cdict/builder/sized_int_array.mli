(** Represent an array of integer of 8, 16 or 24 bits encoded in big-endian. *)

(** Specifies the size and signedness of the integers encoded in the array. *)
type format = I8 | I16 | I24 | U8 | U16 | U24

type t = format * bytes

val mk : format -> int array -> t
(** Create a sized integer array from the integers in [ar] with the specified
    format. Integers from [ar] that do not fit the format are truncated. *)

val mk_detect : ?signed:bool -> int array -> t
(** Create a sized integer array from the integers in [ar]. The format of the
    resulting array is detected from the data.

    [~signed] can be passed to force the use of signed or unsigned integers.
    Integers from [ar] that do not fit the format are truncated, either because
    they do not fit in 24 bits or they are negative and [~signed:false] was
    passed. *)

val get : t -> int -> int
(** [get t i] returns the [i]th integer in the array [t]. *)

val set : t -> int -> int -> unit
(** [set t i v] sets the [i]th integer in the array [t] to [v]. The value of [v]
    is truncated if it doesn't fit the array's format. *)

val format_to_string : format -> string
