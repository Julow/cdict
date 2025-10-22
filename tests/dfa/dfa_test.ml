open Dfa

let words_of_dfa dfa =
  let rec loop acc prefix st =
    let acc = if st.leaves <> [] then prefix :: acc else acc in
    List.fold_left
      (fun acc (c, next) ->
        loop acc (prefix ^ String.make 1 c) (state dfa next))
      acc st.tr
  in
  List.rev (loop [] "" (root_state dfa))

let fail_expected ?(msg = "") pp_a got expected =
  Format.eprintf "%sExpected:@\n{@\n%a@\n}@\nbut got:@\n{@\n%a@\n}@\n%!" msg
    pp_a expected pp_a got;
  failwith "Test failure"

let test words expected =
  let expected = String.trim expected in
  let words = List.sort String.compare words in
  let dfa = of_sorted_list (List.mapi (fun i w -> (w, i)) words) in
  let pp_leaf = Format.pp_print_int in
  let output = String.trim (Format.asprintf "%a" (pp pp_leaf) dfa) in
  if output <> expected then
    fail_expected Format.pp_print_string output expected;
  let words' = words_of_dfa dfa in
  if words' <> words then
    fail_expected ~msg:"Missing words. "
      Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
      words' words

let () =
  test
    [ "pomme"; "pommes"; "poire"; "poires"; "coing" ]
    {|
. 0   'c' 5   'o' 4   'i' 3   'n' 2   'g' 1   <final 4 2 0>
      'p' 10  'o' 9   'i' 8   'r' 7   'e' 6   <final 3 1>
                                              's' (1 seen)
                      'm' 14  'm' (7 seen)
|}

(* The data used in the paper *)
let () =
  test
    [
      "aient";
      "ais";
      "ait";
      "ant";
      "assent";
      "asses";
      "assi";
      "ent";
      "eraient";
      "erais";
      "erait";
      "eras";
      "erez";
      "eriez";
      "erions";
      "erons";
      "eront";
      "es";
      "ez";
      "iez";
      "ions";
      "ons";
      "âmes";
      "âtes";
      "èrent";
      "ées";
      "és";
    ]
    {|
. 0   'a' 20  'i' 19  'e' 18  'n' 17  't' 16  <final 26 25 24 23 22 21 20 19
                                               18 17 16 15 14 13 12 11 10 9 8
                                               7 6 5 4 3 2 1 0>
                      's' (16 seen)
                      't' (16 seen)
              'n' (17 seen)
              's' 29  's' 28  'e' 27  'n' (17 seen)
                                      's' (16 seen)
                              'i' (16 seen)
      'e' 34  'n' (17 seen)
              'r' 40  'a' 39  'i' (19 seen)
                              's' (16 seen)
                      'e' 45  'z' (16 seen)
                      'i' 48  'e' (45 seen)
                              'o' 51  'n' 50  's' (16 seen)
                      'o' 54  'n' 53  's' (16 seen)
                                      't' (16 seen)
              's' (16 seen)
              'z' (16 seen)
      'i' (48 seen)
      'o' (51 seen)
      '\195' 71  '\162' 70  'm' 69  'e' (50 seen)
                            't' (69 seen)
                 '\168' 79  'r' 78  'e' (18 seen)
                 '\169' 82  'e' (50 seen)
                            's' (16 seen)
|}
