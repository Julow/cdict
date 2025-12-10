let main inputs =
  let ds = Build.parse_files_into_cdict_builders inputs in
  List.iter (Cdict_builder.stats Format.std_formatter) ds
