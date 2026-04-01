  $ cdict-tool build -o dict main:words.combined
  Parsing "words.combined"
  Built dictionary "main" (6 words)
  Done.
  $ ls -sh dict
  4.0K dict

  $ cdict-tool query dict sourire heureux smiley joie haha 😀
  found: "\240\159\152\128" freq=0 index=5
  close match: "sourire" distance=2 freq=5 index=4
  close match: "sourire" distance=2 freq=5 index=4
  found: "\240\159\152\132" freq=0 index=8
  close match: "heureux" distance=2 freq=5 index=1
  close match: "heureux" distance=2 freq=5 index=1
  found: "\240\159\152\131" freq=0 index=7
  close match: "smiley" distance=2 freq=5 index=3
  close match: "smiley" distance=2 freq=5 index=3
  found: "\240\159\152\132" freq=0 index=8
  close match: "joie" distance=2 freq=5 index=2
  found: "\240\159\152\134" freq=0 index=9
  close match: "haha" distance=2 freq=5 index=0
  close match: "haha" distance=2 freq=5 index=0
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
  close match: "sourire" distance=1 freq=5 index=4
  not found: "heuxeux"
  close match: "heureux" distance=1 freq=5 index=1
  [2]
