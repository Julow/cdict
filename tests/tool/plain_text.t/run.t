  $ printf "%s\n" foo bar baz > second.txt

  $ cdict-tool build -o dict main:ocaml_manual.txt second:second.txt
  Parsing "ocaml_manual.txt"
  Built dictionary "main" (7984 words)
  Parsing "second.txt"
  Built dictionary "second" (3 words)
  Done.

  $ ls -sh dict
  60K dict

TODO: Some words are not found back due to some encoding issues.

  $ cdict-tool query -q dict --from-file ocaml_manual.txt

  $ cdict-tool query -d main dict foo
  found: "foo" freq=7 index=4223
  prefix: "foo" freq=7 index=4223
  prefix: "fooBar" freq=1 index=4224
  prefix: "footnote" freq=1 index=4225
  close match: "for" distance=1 freq=11 index=4226
  close match: "Foo" distance=1 freq=4 index=1040
  close match: "too" distance=1 freq=4 index=7409
  close match: "fooBar" distance=1 freq=1 index=4224
  close match: "footnote" distance=1 freq=1 index=4225
  close match: "of" distance=2 freq=13 index=5698
  close match: "to" distance=2 freq=13 index=7399
  close match: "to" distance=2 freq=13 index=7399
  close match: "for" distance=2 freq=11 index=4226
  close match: "for" distance=2 freq=11 index=4226
  $ cdict-tool query -d second dict foo
  found: "foo" freq=0 index=2
  prefix: "foo" freq=0 index=2
  close match: "foo" distance=2 freq=0 index=2
