let fail fmt = Format.kasprintf failwith fmt

let create words =
  let words = List.mapi (fun i w -> (w, i)) words in
  let w = Cdict_builder.of_list ~freq:Fun.id words in
  let data = Cdict_builder.to_string w in
  (* Cdict_builder.stats Format.err_formatter w; *)
  (* Hxd_string.pp Hxd.default Format.err_formatter data; *)
  (* Format.pp_print_flush Format.err_formatter (); *)
  Cdict.of_string data

let fpf = Format.fprintf

let expect ?(msg = "") pp_a got expected =
  if got <> expected then (
    Format.eprintf
      "%sExpected: {@\n@[<v>%a@]@\n} but got: {@\n@[<v>%a@]@\n}@\n%!" msg pp_a
      expected pp_a got;
    failwith "Test failure")

let pp_leaf_opt ppf = function
  | Some leaf -> fpf ppf "%d" leaf
  | None -> fpf ppf "<not found>"

let pp_word_leaf_opt ppf (w, l) = fpf ppf "%s %a" w pp_leaf_opt l
let pp_list fmt = Format.(pp_print_list ~pp_sep:pp_print_space) fmt

let find d word =
  let r = Cdict.find d word in
  if r.found then Some (Cdict.freq d r.index) else None

let assert_found d word expected_leaf =
  expect ~msg:"Find returned unexpected value. " pp_leaf_opt (find d word)
    (Some expected_leaf)

let create_and_assert words =
  let d = create words in
  expect ~msg:"create_and_assert. " (pp_list pp_word_leaf_opt)
    (List.map (fun w -> (w, find d w)) words)
    (List.mapi (fun i w -> (w, Some i)) words);
  d

let assert_not_found d word =
  match find d word with
  | Some leaf -> fail "Expected not found but got %d for word %S" leaf word
  | None -> ()

(* Fruit test *)
let () =
  let _ = create_and_assert [ "pomme"; "poire"; "coing"; "poireau" ] in
  ()

(* One empty word *)
let () =
  (* The DFA based dictionary doesn't support the empty word. *)
  let d = create [ "" ] in
  assert_not_found d "";
  assert_not_found d "a"

(* One word *)
let () =
  let d = create [ "a" ] in
  assert_found d "a" 0;
  assert_not_found d "";
  assert_not_found d "b"

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
