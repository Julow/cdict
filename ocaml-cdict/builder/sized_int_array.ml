type format = I8 | I16 | I24 | U8 | U16 | U24
type t = format * bytes

let set_int24_be b i v =
  Bytes.set_int16_be b i (v asr 8);
  Bytes.set_uint8 b (i + 2) (v land 0xFF)

let set_uint24_be b i v =
  Bytes.set_uint16_be b i (v lsr 8);
  Bytes.set_uint8 b (i + 2) (v land 0xFF)

let get_int24_be b i =
  (Bytes.get_int16_be b i lsl 8) lor Bytes.get_uint8 b (i + 2)

let get_uint24_be b i =
  (Bytes.get_uint16_be b i lsl 8) lor Bytes.get_uint8 b (i + 2)

(** Returns the [get] and [set] functions and the size in bytes of the integers
    in the array. *)
let format_specs = function
  | I8 -> (Bytes.get_int8, Bytes.set_int8, 1)
  | I16 -> (Bytes.get_int16_be, Bytes.set_int16_be, 2)
  | I24 -> (get_int24_be, set_int24_be, 3)
  | U8 -> (Bytes.get_uint8, Bytes.set_uint8, 1)
  | U16 -> (Bytes.get_uint16_be, Bytes.set_uint16_be, 2)
  | U24 -> (get_uint24_be, set_uint24_be, 3)

let mk format ar =
  let _get, setf, i_size = format_specs format in
  let ar_len = Array.length ar in
  let b = Bytes.create (ar_len * i_size) in
  for i = 0 to ar_len - 1 do
    setf b (i * i_size) (Array.unsafe_get ar i)
  done;
  (format, b)

let format_of_integer n_abs signed =
  if signed then
    if n_abs <= 0x7F then I8 else if n_abs <= 0x7FFF then I16 else I24
  else if n_abs <= 0xFF then U8
  else if n_abs <= 0xFFFF then U16
  else U24

let rec min_and_max min_ max_ ar i =
  if i >= Array.length ar then (min_, max_)
  else
    let n = ar.(i) in
    min_and_max (Int.min n min_) (Int.max n max_) ar (i + 1)

let detect_format force_signed ar =
  if Array.length ar = 0 then U8
  else
    let first = ar.(0) in
    let min, max_ = min_and_max first first ar 1 in
    let signed = match force_signed with Some s -> s | None -> min < 0 in
    (* [+ 1] so that [-128] because [127] as they both fit on one byte. *)
    let min_abs = if min < 0 then ~-min - 1 else min in
    let max_abs = Int.max min_abs max_ in
    format_of_integer max_abs signed

let mk_detect ?signed ar = mk (detect_format signed ar) ar

let get (format, b) i =
  let get, _set, i_size = format_specs format in
  get b (i * i_size)

let set (format, b) i v =
  let _get, set, i_size = format_specs format in
  set b (i * i_size) v

let format_to_string = function
  | I8 -> "I8"
  | I16 -> "I16"
  | I24 -> "I24"
  | U8 -> "U8"
  | U16 -> "U16"
  | U24 -> "U24"
