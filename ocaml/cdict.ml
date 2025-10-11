type t

external of_string : string -> t = "cdict_of_string_ocaml"
external find : t -> string -> int option = "cdict_find_ocaml"
