open Dfa

let test words expected =
  let expected = String.trim expected in
  let output =
    let dfa = of_sorted_list (List.sort String.compare words) in
    String.trim (Format.asprintf "%a" pp dfa)
  in
  if output <> expected then (
    Format.eprintf "Expected:@\n{@\n%s@\n}@\nbut got:@\n{@\n%s@\n}@\n%!"
      expected output;
    failwith "Test failure")

let () =
  test
    [ "pomme"; "pommes"; "poire"; "poires"; "coing" ]
    {|
. 0   'c' 5   'o' 4   'i' 3   'n' 2   'g' 1   <final>
      'p' 10  'o' 9   'i' 8   'r' 7   'e' 6   <final>
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
. 0   'a' 20  'i' 19  'e' 18  'n' 17  't' 16  <final>
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
