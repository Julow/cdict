type t
type index = private int

type ptr
(** Internal pointer in the dictionary. *)

type result = {
  found : bool;  (** Whether the word is recognized. *)
  index : index;
      (** Unique index of the word, can be used to lookup word metadata. *)
}

external of_string : string -> t = "cdict_of_string_ocaml"
(** Load a dictionary stored in a string. Use the library [cdict.builder] to
    construct this dictionary. *)

external find : t -> string -> result = "cdict_find_ocaml"
(** Lookup a word in the dictionary. *)

external freq : t -> index -> int = "cdict_freq_ocaml"
(** Query the frequency associated to a word. *)

external word : t -> index -> string = "cdict_word_ocaml"
(** Retrieve the word at the given index. *)
