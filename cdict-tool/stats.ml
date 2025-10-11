let main inputs =
  let d = Build.parse_files_into_cdict_writer inputs in
  Cdict_writer.stats Format.std_formatter d
