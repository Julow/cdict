type leaf = { freq : int }

let encode_leaf l = l.freq

let parse_aosp_combined ~fname =
  let open Aosp_parser in
  let wordlist = In_channel.with_open_text fname (parse ~fname) in
  List.map (fun w -> (w.w, { freq = w.w_freq })) wordlist.words

let parse_newline_separated ~fname =
  let wordlist = In_channel.(with_open_text fname input_lines) in
  let counts = Hashtbl.create (List.length wordlist) in
  List.iter
    (fun w ->
      let c = try Hashtbl.find counts w with Not_found -> 0 in
      Hashtbl.replace counts w (c + 1))
    wordlist;
  Hashtbl.fold (fun w freq acc -> (w, { freq }) :: acc) counts []

let parse_file fname =
  Printf.printf "Parsing %S\n%!" fname;
  match Filename.extension fname with
  | ".combined" -> parse_aosp_combined ~fname
  | _ -> parse_newline_separated ~fname

let main output inputs =
  try
    let words = List.concat_map parse_file inputs in
    Printf.printf "Generating a %d words dictionary.\n%!" (List.length words);
    let d = Cdict_writer.of_list words in
    Out_channel.with_open_bin output (fun out_chan ->
        Cdict_writer.output d ~encode_leaf out_chan);
    Printf.printf "Done.\n%!"
  with Failure msg ->
    Printf.eprintf "Error: %s\n%!" msg;
    exit 1

open Cmdliner

let arg_output =
  let doc = "Output file" in
  Arg.(required & opt (some string) None & info ~doc [ "o" ])

let arg_inputs =
  let doc = "Input files" in
  Arg.(value & pos_all file [] & info ~doc [])

let cmd =
  let term = Term.(const main $ arg_output $ arg_inputs) in
  let doc =
    "Compile dictionaries for libcdict from text files. Supported formats are:\n\n\
    \  - AOSP word lists, if the file extension is .combined.\n\
    \  - Plain text files with one word per line, for any other file \
     extension.\n\n\
     The leaves of contain the frequency of the word as specified in the AOSP \
     word list or counted in the plain text word list."
  in
  let info = Cmd.info "cdict-tool" ~version:"%%VERSION%%" ~doc in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
