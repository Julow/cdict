module Freq = Cdict_builder.Freq

let partition data =
  let t = Freq.of_int_array data in
  Array.mapi (fun i v -> (v, Freq.get t i)) data

let () = Random.self_init ()
let rand = Random.int

let expect pp expected got =
  if expected <> got then (
    Format.eprintf
      "@[<v>Test failure. Expected:@;\
       <1 2>@[<v>%a@]@ but got:@;\
       <1 2>@[<v>%a@]@]@\n"
      pp expected pp got;
    assert false)

let pp_result =
  let pp ppf (v, b) = Format.fprintf ppf "v=%d, bucket=%d" v b in
  Format.(pp_print_array ~pp_sep:pp_print_space pp)

let expect' exp got = expect pp_result exp got

let () =
  for _ = 1 to 1000 do
    let size = rand 64 in
    let data = Array.init size (fun _ -> rand 1_000_000) in
    let r = partition data in
    (* Check that frequencies are in range. *)
    Array.iter (fun (_, b) -> assert (b >= 0 && b <= 0xF)) r;
    (* Check that the order is preserved. *)
    Array.sort (fun (a, _) (b, _) -> compare a b) r;
    Array.iteri
      (fun i (_, b) -> if i > 0 then assert (b >= snd r.(i - 1)))
      r
  done

let () = expect' (partition [||]) [||]
let () = expect' (partition [| 0 |]) [| (0, 0) |]
let () = expect' (partition [| 1 |]) [| (1, 0) |]
let () = expect' (partition [| 5; 5; 5 |]) [| (5, 0); (5, 0); (5, 0) |]
