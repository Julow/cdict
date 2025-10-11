let main inputs =
  let d = Build.parse_files_into_cdict_builder inputs in
  Cdict_builder.stats Format.std_formatter d
