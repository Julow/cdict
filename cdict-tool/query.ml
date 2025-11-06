let open_dict fname =
  let data = In_channel.(with_open_bin fname input_all) in
  Cdict.of_string data

let queries_from_file = function
  | Some fname -> In_channel.(with_open_text fname input_lines)
  | None -> []

let query_distance dict ~dist q =
  Cdict.distance dict q ~dist ~count:5
  |> Array.iter (fun idx ->
         Printf.printf "close match: %S distance=%d freq=%d\n"
           (Cdict.word dict idx) dist (Cdict.freq dict idx))

let query ~quiet dict q =
  let r : Cdict.result = Cdict.find dict q in
  if not quiet then (
    if r.found then
      let freq = Cdict.freq dict r.index in
      Printf.printf "found: %S freq=%d index=%d\n" q freq (r.index :> int)
    else Printf.printf "not found: %S\n" q;
    let prefixes_idx = Cdict.suffixes dict r 5 in
    Array.iter
      (fun idx ->
        Printf.printf "prefix: %S freq=%d index=%d\n" (Cdict.word dict idx)
          (Cdict.freq dict idx)
          (idx :> int))
      prefixes_idx;
    query_distance dict ~dist:1 q;
    query_distance dict ~dist:2 q);
  if r.found then 0 else 1

let main quiet from_file dict_fname queries =
  let dict = open_dict dict_fname in
  let queries = queries @ queries_from_file from_file in
  exit (List.fold_left (fun n q -> query ~quiet dict q + n) 0 queries)
