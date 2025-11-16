  $ cdict-tool build -o dict ocaml_manual.txt
  Parsing "ocaml_manual.txt"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  80K dict

TODO: Some words are not found back due to some encoding issues.

  $ cdict-tool query -q dict --from-file ocaml_manual.txt
  [3]
