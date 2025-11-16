  $ cdict-tool build -o dict words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  80K dict

  $ cdict-tool query dict type module function value match
  found: "type" freq=15 index=7526
  prefix: "typexpr" freq=15 index=7540
  prefix: "types" freq=15 index=7537
  prefix: "typeconstr" freq=14 index=7530
  prefix: "typename" freq=0 index=7535
  prefix: "typecheck" freq=7 index=7527
  close match: "Type" distance=1 freq=14
  close match: "Type" distance=1 freq=14
  close match: "hyperfine" distance=1 freq=10
  close match: "hyperref" distance=1 freq=14
  close match: "typable" distance=1 freq=0
  close match: "style" distance=2 freq=14
  close match: "superclass" distance=2 freq=0
  close match: "pending" distance=2 freq=12
  close match: "pessimization" distance=2 freq=0
  close match: "supertype" distance=2 freq=11
  found: "module" freq=15 index=5399
  prefix: "moduleref" freq=5 index=5403
  prefix: "moduleexamples" freq=10 index=5401
  prefix: "modulename" freq=5 index=5402
  prefix: "modulealias" freq=5 index=5400
  prefix: "module" freq=15 index=5399
  close match: "modular" distance=1 freq=9
  close match: "modulus" distance=1 freq=7
  close match: "Module1" distance=1 freq=7
  close match: "Module" distance=1 freq=14
  close match: "moduleexamples" distance=1 freq=10
  close match: "modules" distance=2 freq=15
  close match: "moduleexamples" distance=2 freq=10
  close match: "Module1" distance=2 freq=7
  close match: "Module" distance=2 freq=14
  close match: "moduleref" distance=2 freq=5
  found: "function" freq=15 index=4302
  prefix: "functions" freq=15 index=4306
  prefix: "functional" freq=14 index=4303
  prefix: "function" freq=15 index=4302
  prefix: "functionals" freq=7 index=4305
  prefix: "functionality" freq=5 index=4304
  close match: "Functional" distance=1 freq=7
  close match: "Function" distance=1 freq=10
  close match: "Functionals" distance=1 freq=0
  close match: "Function" distance=1 freq=10
  close match: "functionals" distance=2 freq=7
  close match: "functionality" distance=2 freq=5
  close match: "Functionals" distance=2 freq=0
  close match: "Function" distance=2 freq=10
  close match: "function\226\128\153s" distance=2 freq=5
  found: "value" freq=15 index=7722
  prefix: "value" freq=15 index=7722
  prefix: "valuerestriction" freq=0 index=7723
  close match: "vanilla" distance=1 freq=5
  close match: "vanishes" distance=1 freq=0
  close match: "Value" distance=1 freq=8
  close match: "Value" distance=1 freq=8
  close match: "values" distance=1 freq=15
  close match: "glue" distance=2 freq=0
  close match: "gnu" distance=2 freq=0
  close match: "Value" distance=2 freq=8
  close match: "blue" distance=2 freq=5
  close match: "evaluating" distance=2 freq=8
  found: "match" freq=15 index=5267
  prefix: "match" freq=15 index=5267
  prefix: "matched" freq=14 index=5268
  prefix: "matching" freq=15 index=5270
  prefix: "matches" freq=14 index=5269
  close match: "matched" distance=1 freq=14
  close match: "matches" distance=1 freq=14
  close match: "batch" distance=1 freq=11
  close match: "Batch" distance=1 freq=0
  close match: "matchings" distance=1 freq=11
  close match: "outch" distance=2 freq=10
  close match: "matched" distance=2 freq=14
  close match: "batch" distance=2 freq=11
  close match: "Batch" distance=2 freq=0
  close match: "matches" distance=2 freq=14

  $ cdict-tool query dict overload enum defensive coding
  not found: "overload"
  close match: "overloading" distance=1 freq=0
  close match: "overly" distance=2 freq=5
  close match: "overlook" distance=2 freq=0
  close match: "overrides" distance=2 freq=7
  close match: "overhead" distance=2 freq=9
  close match: "overridden" distance=2 freq=8
  not found: "enum"
  close match: "every" distance=1 freq=13
  close match: "ever" distance=1 freq=8
  close match: "numbers" distance=1 freq=14
  close match: "num" distance=1 freq=14
  close match: "everything" distance=1 freq=10
  close match: "vy" distance=2 freq=7
  close match: "unambiguously" distance=2 freq=5
  close match: "umask" distance=2 freq=0
  close match: "Jump" distance=2 freq=0
  close match: "unroll" distance=2 freq=11
  not found: "defensive"
  not found: "coding"
  close match: "coming" distance=1 freq=5
  close match: "Coding" distance=1 freq=0
  close match: "moving" distance=2 freq=5
  close match: "scoping" distance=2 freq=0
  close match: "Coding" distance=2 freq=0
  close match: "owing" distance=2 freq=0
  close match: "hash" distance=2 freq=14
  [4]

Prefix search:

  $ cdict-tool query dict typ
  found: "typ" freq=15 index=7524
  prefix: "typ" freq=15 index=7524
  prefix: "typing" freq=13 index=7544
  prefix: "typexr" freq=5 index=7541
  prefix: "typically" freq=14 index=7543
  prefix: "typexpr" freq=15 index=7540
  close match: "mypoint" distance=1 freq=7
  close match: "myprog" distance=1 freq=13
  close match: "hyperref" distance=1 freq=14
  close match: "hyphen" distance=1 freq=0
  close match: "Types" distance=1 freq=0
  close match: "uppercase" distance=2 freq=13
  close match: "upper" distance=2 freq=11
  close match: "pp" distance=2 freq=13
  close match: "p" distance=2 freq=15
  close match: "unpack" distance=2 freq=5

Stats:

  $ cdict-tool stats words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Nodes: 4918
  Branch nodes: 2262
    With size:
    |  0: 1    |  2: 1287 |  3: 446  |  4: 209  |  5: 80   |  6: 49  
    |  7: 51   |  8: 24   |  9: 20   | 10: 19   | 11: 13   | 12: 13  
    | 13: 7    | 14: 6    | 15: 5    | 16: 7    | 17: 2    | 18: 3   
    | 19: 4    | 20: 1    | 21: 4    | 22: 3    | 23: 1    | 24: 3   
    | 25: 1    | 26: 1    | 27: 1    | 68: 1    |
    Transitions: 7597
      | Final: 1995 | Non-final: 5602 |
      With numbers length in bytes: |  1: 7597 |
  Prefix nodes: 2523
    Followed by:
    | Branches: 2116 | Prefix: 407  |
    With size:
    |  1: 550  |  2: 523  |  3: 505  |  4: 358  |  5: 246  |  6: 145 
    |  7: 88   |  8: 54   |  9: 17   | 10: 22   | 11: 5    | 12: 1   
    | 13: 4    | 15: 1    | 17: 1    | 19: 1    | 27: 1    | 61: 1    |
    Transitions: 2523
      | Final: 2061 | Non-final: 462  |
      With numbers length in bytes: |  1: 2523 |
  Number nodes: 133
    With next:
    | Branches: 106  | Prefix: 27   |
    With final: | Final: 86   | Non-final: 47   |
  Freq: 7984
    With value:
    |  0: 2611 |  5: 1156 |  7: 666  |  8: 453  |  9: 342  | 10: 281 
    | 11: 531  | 12: 490  | 13: 468  | 14: 492  | 15: 494  |
