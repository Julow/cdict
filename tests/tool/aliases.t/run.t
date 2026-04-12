  $ cdict-tool build -o dict main:words.combined
  Parsing "words.combined"
  Built dictionary "main" (6 words)
  Done.
  $ ls -sh dict
  4.0K dict

  $ cdict-tool query dict sourire heureux smiley joie haha 😀
  found: "\240\159\152\128" freq=0 index=5
  close match: "\240\159\152\128" distance=2 freq=0 index=5
  close match: "\240\159\152\128" distance=2 freq=0 index=5
  found: "\240\159\152\132" freq=0 index=8
  close match: "\240\159\152\132" distance=2 freq=0 index=8
  close match: "\240\159\152\132" distance=2 freq=0 index=8
  found: "\240\159\152\131" freq=0 index=7
  close match: "\240\159\152\131" distance=2 freq=0 index=7
  close match: "\240\159\152\131" distance=2 freq=0 index=7
  found: "\240\159\152\132" freq=0 index=8
  close match: "\240\159\152\132" distance=2 freq=0 index=8
  found: "\240\159\152\134" freq=0 index=9
  close match: "\240\159\152\134" distance=2 freq=0 index=9
  close match: "\240\159\152\134" distance=2 freq=0 index=9
  found: "\240\159\152\128" freq=0 index=5
  close match: "\240\159\152\130" distance=1 freq=0 index=6
  close match: "\240\159\152\131" distance=1 freq=0 index=7
  close match: "\240\159\152\132" distance=1 freq=0 index=8
  close match: "\240\159\152\134" distance=1 freq=0 index=9
  close match: "\240\159\152\128" distance=2 freq=0 index=5
  close match: "\240\159\152\128" distance=2 freq=0 index=5
  close match: "\240\159\152\128" distance=2 freq=0 index=5
  close match: "\240\159\152\130" distance=2 freq=0 index=6
  close match: "\240\159\152\131" distance=2 freq=0 index=7

  $ cdict-tool query dict sour heuxeux
  not found: "sour"
  prefix: "\240\159\152\128" freq=0 index=5
  close match: "\240\159\152\128" distance=1 freq=0 index=5
  not found: "heuxeux"
  close match: "\240\159\152\132" distance=1 freq=0 index=8
  [2]

The combination of substitutions and shortcuts creates alias chains:
(eg. sorire -> sourire -> 😀)

  $ cdict-tool build -s subst.json -o subst.dict main:words.combined
  Parsing "words.combined"
  Built dictionary "main" (6 words)
  Done.

  $ cdict-tool query subst.dict so
  not found: "so"
  prefix: "sourire" freq=7 index=6
  prefix: "\240\159\152\128" freq=0 index=7
  close match: "sourire" distance=1 freq=7 index=6
  close match: "\240\159\152\128" distance=1 freq=0 index=7
  close match: "sourire" distance=2 freq=7 index=6
  close match: "\240\159\152\128" distance=2 freq=0 index=7
  close match: "\240\159\152\131" distance=2 freq=0 index=9
  close match: "\240\159\152\131" distance=2 freq=0 index=9
  close match: "\240\159\152\132" distance=2 freq=0 index=10
  [1]
