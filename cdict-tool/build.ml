type word = Aosp_parser.word = {
  w : string;
  w_freq : int;
  w_shortcuts : string list;
}

let shortcut_to_word w = (w, { w; w_freq = 1; w_shortcuts = [] })

let build ~dict_name words =
  Printf.printf "Built dictionary %S (%d words)\n%!" dict_name
    (List.length words);
  (* Add all the shortcuts to the dictionary. *)
  let words =
    List.fold_left
      (fun acc (_, w) ->
        List.rev_append (List.rev_map shortcut_to_word w.w_shortcuts) acc)
      words words
    |> List.rev
    (* Ensure that the original words appear first in the list or they would get
       shadowed by shortcuts. *)
  in
  Cdict_builder.of_list ~name:dict_name
    ~freq:(fun w -> w.w_freq)
    ~alias:(fun w -> List.nth_opt w.w_shortcuts 0)
    words

let parse_aosp_combined ~fname ~dict_name =
  let open Aosp_parser in
  let wordlist = In_channel.with_open_text fname (parse ~fname) in
  let words = List.rev_map (fun w -> (w.w, w)) wordlist.words in
  build ~dict_name words

let parse_newline_separated ~fname ~dict_name =
  let wordlist = In_channel.(with_open_text fname input_lines) in
  let counts = Hashtbl.create (List.length wordlist) in
  List.iter
    (fun w ->
      let c = try Hashtbl.find counts w with Not_found -> 0 in
      Hashtbl.replace counts w (c + 1))
    wordlist;
  let words =
    Hashtbl.fold
      (fun w w_freq acc -> (w, { w; w_freq; w_shortcuts = [] }) :: acc)
      counts []
  in
  build ~dict_name words

let parse_file fname ~dict_name =
  Printf.printf "Parsing %S\n%!" fname;
  match Filename.extension fname with
  | ".combined" -> parse_aosp_combined ~fname ~dict_name
  | _ -> parse_newline_separated ~fname ~dict_name

let parse_files_into_cdict_builders inputs =
  if not (List.exists (fun (n, _) -> n = "main") inputs) then
    Format.eprintf "Warning: No dictionary named \"main\" specified@\n";
  List.map (fun (dict_name, path) -> parse_file path ~dict_name) inputs

let main output inputs =
  try
    let ds = parse_files_into_cdict_builders inputs in
    Out_channel.with_open_bin output (fun out_chan ->
        Cdict_builder.output ds out_chan);
    Printf.printf "Done.\n%!"
  with Failure msg ->
    Printf.eprintf "Error: %s\n%!" msg;
    exit 1
