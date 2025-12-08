let parse_aosp_combined ~fname =
  let open Aosp_parser in
  let wordlist = In_channel.with_open_text fname (parse ~fname) in
  List.map (fun w -> (w.w, w.w_freq)) wordlist.words

let parse_newline_separated ~fname =
  let wordlist = In_channel.(with_open_text fname input_lines) in
  let counts = Hashtbl.create (List.length wordlist) in
  List.iter
    (fun w ->
      let c = try Hashtbl.find counts w with Not_found -> 0 in
      Hashtbl.replace counts w (c + 1))
    wordlist;
  Hashtbl.fold (fun w freq acc -> (w, freq) :: acc) counts []

let parse_file fname =
  Printf.printf "Parsing %S\n%!" fname;
  match Filename.extension fname with
  | ".combined" -> parse_aosp_combined ~fname
  | _ -> parse_newline_separated ~fname

let parse_files_into_cdict_builder inputs =
  let words = List.concat_map parse_file inputs in
  Printf.printf "Generating a %d words dictionary.\n%!" (List.length words);
  Cdict_builder.of_list ~name:"main" ~freq:(fun f -> f) words

let main output inputs =
  try
    let d = parse_files_into_cdict_builder inputs in
    Out_channel.with_open_bin output (fun out_chan ->
        Cdict_builder.output [ d ] out_chan);
    Printf.printf "Done.\n%!"
  with Failure msg ->
    Printf.eprintf "Error: %s\n%!" msg;
    exit 1
