let open_dict fname =
  let data = In_channel.(with_open_bin fname input_all) in
  Cdict.of_string data

let queries_from_file = function
  | Some fname -> In_channel.(with_open_text fname input_lines)
  | None -> []

let query ~quiet dict q =
  match Cdict.find dict q with
  | Some freq ->
      if not quiet then Printf.printf "found: %S freq=%d\n" q freq;
      0
  | None ->
      if not quiet then Printf.printf "not found: %S\n" q;
      1

let main quiet from_file dict_fname queries =
  let dict = open_dict dict_fname in
  let queries = queries @ queries_from_file from_file in
  exit (List.fold_left (fun n q -> query ~quiet dict q + n) 0 queries)
