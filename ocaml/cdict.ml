type t
type index = private int

type ptr
(** Internal pointer in the dictionary. *)

type result = {
  found : bool;  (** Whether the word is recognized. *)
  index : index;
      (** Unique index of the word, can be used to lookup word metadata. *)
  prefix_ptr : ptr;
      (** Internal pointer used to list words starting with a prefix. *)
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

external list_prefix : t -> result -> int -> index array
  = "cdict_list_prefix_ocaml"
(** List words that starts with the query passed to {!find}. This can be called
    even if [result.found] is false. The returned array cannot contain more than
    [len] elements but might be smaller. *)
