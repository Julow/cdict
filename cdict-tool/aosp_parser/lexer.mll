{
open Parser

(* Tracking line numbers without allocating [Lexing.position]s. *)
let fname = ref ""
let linen = ref 0

let start fname_ =
  fname := fname_;
  linen := 1

let warn msg =
  Printf.eprintf ("Warning: While reading %S at line %d: " ^^ msg ^^ ".\n%!")
    !fname !linen
}

let sp = [' ' '\t']*
let eol_char = ['\n' '\r']
let field_name = ['a'-'z' 'A'-'Z' '_']+
let field_value = (_ # [','] # eol_char)*
let not_eol = (_ # eol_char)

rule fields acc = parse
  | ',' (field_name as f) '=' (field_value as v) {
      fields ((f, v) :: acc) lexbuf }
  | not_eol {
      warn "Invalid field syntax"; discard_line lexbuf; acc }
  | eol_char | eof { incr linen; acc }

and line_value = parse
  | field_value as first_field { first_field, fields [] lexbuf }

and line = parse
  | sp "dictionary=" { Dictionary (line_value lexbuf) }
  | sp "word=" { Word (line_value lexbuf) }
  | sp "shortcut=" { Shortcut (line_value lexbuf) }
  | sp field_name* {
      warn "Unknown line prefix"; discard_line lexbuf; line lexbuf }
  | eof { Eof }

and discard_line = parse
  | not_eol* eol_char { incr linen }
