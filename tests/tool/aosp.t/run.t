  $ cdict-tool build -o dict words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  88K dict

  $ cdict-tool query dict type module function value match
  found: "type" freq=15 index=7526
  prefix: "typexpr" freq=15 index=7540
  prefix: "types" freq=15 index=7537
  prefix: "typeconstr" freq=14 index=7530
  prefix: "typename" freq=0 index=7535
  prefix: "typecheck" freq=7 index=7527
  close match: "typable" distance=1 freq=0
  close match: "typtr" distance=1 freq=10
  close match: "Type" distance=1 freq=14
  close match: "Type" distance=1 freq=14
  close match: "typically" distance=1 freq=14
  close match: "typexr" distance=2 freq=5
  close match: "typexpr" distance=2 freq=15
  close match: "pending" distance=2 freq=12
  close match: "pessimization" distance=2 freq=0
  close match: "types" distance=2 freq=15
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
  prefix: "valuerestriction" freq=0 index=7723
  prefix: "value" freq=15 index=7722
  close match: "vanilla" distance=1 freq=5
  close match: "vanishes" distance=1 freq=0
  close match: "Value" distance=1 freq=8
  close match: "Value" distance=1 freq=8
  close match: "values" distance=1 freq=15
  close match: "values" distance=2 freq=15
  close match: "valuerestriction" distance=2 freq=0
  close match: "Value" distance=2 freq=8
  close match: "archive" distance=2 freq=0
  close match: "valuerestriction" distance=2 freq=0
  found: "match" freq=15 index=5267
  prefix: "matching" freq=15 index=5270
  prefix: "matches" freq=14 index=5269
  prefix: "match" freq=15 index=5267
  prefix: "matched" freq=14 index=5268
  close match: "matchings" distance=1 freq=11
  close match: "matching" distance=1 freq=15
  close match: "Match" distance=1 freq=14
  close match: "Batch" distance=1 freq=0
  close match: "matched" distance=1 freq=14
  close match: "reader" distance=2 freq=7
  close match: "pipe" distance=2 freq=5
  close match: "Match" distance=2 freq=14
  close match: "Batch" distance=2 freq=0
  close match: "pipe" distance=2 freq=5

  $ cdict-tool query dict overload enum defensive coding
  not found: "overload"
  close match: "overloading" distance=1 freq=0
  close match: "overly" distance=2 freq=5
  close match: "overlook" distance=2 freq=0
  close match: "overrides" distance=2 freq=7
  close match: "overhead" distance=2 freq=9
  close match: "overridden" distance=2 freq=8
  not found: "enum"
  close match: "vnum" distance=1 freq=12
  close match: "everything" distance=1 freq=10
  close match: "numbers" distance=1 freq=14
  close match: "num" distance=1 freq=14
  close match: "every" distance=1 freq=13
  close match: "void" distance=2 freq=13
  close match: "vnum" distance=2 freq=12
  close match: "Jump" distance=2 freq=0
  close match: "umask" distance=2 freq=0
  close match: "vy" distance=2 freq=7
  not found: "defensive"
  not found: "coding"
  close match: "coming" distance=1 freq=5
  close match: "Coding" distance=1 freq=0
  close match: "loading" distance=2 freq=12
  close match: "loading" distance=2 freq=12
  close match: "Coding" distance=2 freq=0
  close match: "Coding" distance=2 freq=0
  close match: "hiding" distance=2 freq=7
  [4]

Prefix search:

  $ cdict-tool query dict typ
  found: "typ" freq=15 index=7524
  prefix: "typ" freq=15 index=7524
  prefix: "typing" freq=13 index=7544
  prefix: "typexr" freq=5 index=7541
  prefix: "typically" freq=14 index=7543
  prefix: "typexpr" freq=15 index=7540
  close match: "typtr" distance=1 freq=10
  close match: "typexr" distance=1 freq=5
  close match: "TypEq" distance=1 freq=12
  close match: "Typ" distance=1 freq=12
  close match: "typable" distance=1 freq=0
  close match: "zip" distance=2 freq=13
  close match: "zip" distance=2 freq=13
  close match: "pack" distance=2 freq=14
  close match: "p" distance=2 freq=15
  close match: "z3" distance=2 freq=11

Stats:

  $ cdict-tool stats words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Nodes: 4918
  Branch nodes: 742
    With 'next' nodes:
    |  1: 649  |  2: 46   |  3: 26   |  4: 9    |  5: 5    |  6: 4   
    |  7: 3    |
    Ranges: 925:
      |  0: 1    |  1: 72   |  2: 533  |  3: 122  |  4: 38   |  5: 32  
      |  6: 24   |  7: 23   |  8: 7    |  9: 15   | 10: 15   | 11: 8   
      | 12: 5    | 13: 8    | 14: 1    | 15: 6    | 16: 3    | 17: 4   
      | 18: 1    | 19: 2    | 20: 1    | 21: 2    | 26: 1    | 27: 1    |
    Transitions: 3105
      | Final: 885  | Non-final: 2220 |
      With numbers length in bytes: |  1: 3105 |
  Prefix nodes: 2523
    Followed by:
    | Branches: 1641 | Btree: 475  | Prefix: 407  |
    With size:
    |  1: 550  |  2: 523  |  3: 505  |  4: 358  |  5: 246  |  6: 145 
    |  7: 88   |  8: 54   |  9: 17   | 10: 22   | 11: 5    | 12: 1   
    | 13: 4    | 15: 1    | 17: 1    | 19: 1    | 27: 1    | 61: 1    |
    Transitions: 2523
      | Final: 2061 | Non-final: 462  |
      With numbers length in bytes: |  1: 2523 |
  Btree nodes: 1520
    With size:
    |  2: 802  |  3: 355  |  4: 189  |  5: 65   |  6: 44   |  7: 42  
    |  8: 23   |
    Transitions: 4492
      | Final: 1110 | Non-final: 3382 |
      With numbers length in bytes: |  1: 4492 |
  Number nodes: 133
    With next:
    | Branches: 64   | Btree: 42   | Prefix: 27   |
    With final: | Final: 86   | Non-final: 47   |
  Freq: 7984
    With value:
    |  0: 2611 |  5: 1156 |  7: 666  |  8: 453  |  9: 342  | 10: 281 
    | 11: 531  | 12: 490  | 13: 468  | 14: 492  | 15: 494  |
