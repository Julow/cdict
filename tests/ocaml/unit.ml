let fail fmt = Format.kasprintf failwith fmt

let create words =
  let words = List.mapi (fun i w -> (w, i)) words in
  let encode_leaf i = i in
  let w = Cdict_builder.of_list words in
  let data = Cdict_builder.to_string w ~encode_leaf in
  (* Cdict_builder.stats Format.err_formatter w; *)
  (* Hxd_string.pp Hxd.default Format.err_formatter data; *)
  (* Format.pp_print_flush Format.err_formatter (); *)
  Cdict.of_string data

let assert_found d word expected_leaf =
  match Cdict.find d word with
  | Some leaf ->
      if leaf <> expected_leaf then
        fail "Expected %d but got %d for word %S" expected_leaf leaf word
  | None -> fail "Expected word not found"

let create_and_assert words =
  let d = create words in
  List.iteri (fun i w -> assert_found d w i) words;
  d

let assert_not_found d word =
  match Cdict.find d word with
  | Some leaf -> fail "Expected not found but got %d for word %S" leaf word
  | None -> ()

(* Fruit test *)
let () =
  let d = create [ "pomme"; "poire"; "coing"; "poireau" ] in
  assert_found d "coing" 2;
  (* Own branch from the root *)
  assert_found d "poire" 1;
  (* Branch with leaf *)
  assert_found d "poireau" 3

(* One empty word *)
let () =
  let d = create [ "" ] in
  assert_found d "" 0;
  assert_not_found d "a"

(* One word *)
let () =
  let d = create [ "a" ] in
  assert_found d "a" 0;
  assert_not_found d ""

(* Empty dict *)
let () =
  let d = create [] in
  assert_not_found d "";
  assert_not_found d "a"

(* Btree node *)
let () =
  let _ = create_and_assert [ "y"; "z"; "0"; "1"; "2" ] in
  let rec loop ws =
    let d = create_and_assert ws in
    assert_not_found d "d";
    match ws with [] -> () | _ :: tl -> loop tl
  in
  loop [ "a"; "b"; "c"; "x"; "y"; "z"; "0"; "1"; "2" ]
