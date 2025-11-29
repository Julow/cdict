open Cmdliner

let cmd_build =
  let arg_output =
    let doc = "Output file." in
    Arg.(required & opt (some string) None & info ~doc [ "o" ])
  in
  let arg_inputs =
    let doc = "Input files." in
    Arg.(value & pos_all file [] & info ~doc [])
  in
  let term = Term.(const Build.main $ arg_output $ arg_inputs) in
  let doc =
    "Compile dictionaries for libcdict from text files. Supported formats are:\n\n\
    \  - AOSP word lists, if the file extension is .combined.\n\
    \  - Plain text files with one word per line, for any other file \
     extension.\n\n\
     The leaves of contain the frequency of the word as specified in the AOSP \
     word list or counted in the plain text word list."
  in
  Cmd.(v (info "build" ~doc)) term

let cmd_query =
  let opt_quiet =
    let doc = "Suppress output." in
    Arg.(value & flag & info ~doc [ "q" ])
  in
  let opt_from_file =
    let doc = "Read the words to query from a file." in
    Arg.(value & opt (some file) None & info ~doc [ "from-file" ])
  in
  let arg_dict =
    let doc = "Dictionary to query." in
    Arg.(required & pos 0 (some file) None & info ~doc ~docv:"DICT" [])
  in
  let arg_queries =
    let doc = "Words to look for in the dictionary." in
    Arg.(value & pos_right 0 string [] & info ~doc ~docv:"WORDS" [])
  in
  let term =
    Term.(const Query.main $ opt_quiet $ opt_from_file $ arg_dict $ arg_queries)
  in
  let doc =
    "Query the specified words in the given dictionary. Exits with status \
     equal to the number of words not found in the dictionary."
  in
  Cmd.(v (info "query" ~doc)) term

let cmd_stats =
  let arg_inputs =
    let doc = "The same files that the 'build' command accepts." in
    Arg.(value & pos_all file [] & info ~doc [])
  in
  let term = Term.(const Stats.main $ arg_inputs) in
  let doc = "Build a dictionary and print stats about its structure." in
  Cmd.(v (info "stats" ~doc)) term

let cmd_format_version =
  let run () = Printf.printf "%d\n%!" (Cdict.format_version ()) in
  let term = Term.(const run $ const ()) in
  let doc = "Print the version of the dictionary format." in
  Cmd.(v (info "format-version" ~doc)) term

let cmd =
  let doc = "Build dictionaries from libcdict." in
  let info = Cmd.info "cdict-tool" ~version:"%%VERSION%%" ~doc in
  Cmd.group info [ cmd_build; cmd_query; cmd_stats; cmd_format_version ]

let () = exit (Cmd.eval cmd)
