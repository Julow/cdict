  $ cdict-tool build -o dict words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  164K dict

  $ cdict-tool query dict type module function value match
  found: "type" freq=2070
  found: "module" freq=1013
  found: "function" freq=738
  found: "value" freq=746
  found: "match" freq=247

  $ cdict-tool query dict overload enum defensive coding
  not found: "overload"
  not found: "enum"
  not found: "defensive"
  not found: "coding"
  [4]
