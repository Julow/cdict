  $ cdict-tool build -o dict words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  72K dict

  $ cdict-tool query dict type module function value match
  found: "type" freq=15 index=7526
  prefix: "typexpr" freq=15 index=7540
  prefix: "types" freq=15 index=7537
  prefix: "typeconstr" freq=14 index=7530
  prefix: "typename" freq=0 index=7535
  prefix: "typecheck" freq=7 index=7527
  close match: "Type" distance=1 freq=14 index=2115
  close match: "typexpr" distance=1 freq=15 index=7540
  close match: "typer" distance=1 freq=0 index=7536
  close match: "type" distance=1 freq=15 index=7526
  close match: "types" distance=1 freq=15 index=7537
  close match: "style" distance=2 freq=14 index=7108
  close match: "mytype" distance=2 freq=9 index=5485
  close match: "Type" distance=2 freq=14 index=2115
  close match: "type" distance=2 freq=15 index=7526
  close match: "Style" distance=2 freq=0 index=1996
  found: "module" freq=15 index=5399
  prefix: "moduleref" freq=5 index=5403
  prefix: "moduleexamples" freq=10 index=5401
  prefix: "modulename" freq=5 index=5402
  prefix: "modulealias" freq=5 index=5400
  prefix: "module" freq=15 index=5399
  close match: "modules" distance=1 freq=15 index=5404
  close match: "moduleexamples" distance=1 freq=10 index=5401
  close match: "mdule" distance=1 freq=11 index=5286
  close match: "Module" distance=1 freq=14 index=1466
  close match: "moduleref" distance=1 freq=5 index=5403
  close match: "middle" distance=2 freq=11 index=5334
  close match: "mobile" distance=2 freq=0 index=5376
  close match: "mdule" distance=2 freq=11 index=5286
  close match: "Module" distance=2 freq=14 index=1466
  close match: "models" distance=2 freq=9 index=5386
  found: "function" freq=15 index=4302
  prefix: "functions" freq=15 index=4306
  prefix: "functional" freq=14 index=4303
  prefix: "function" freq=15 index=4302
  prefix: "functionals" freq=7 index=4305
  prefix: "functionality" freq=5 index=4304
  close match: "functionals" distance=1 freq=7 index=4305
  close match: "functionality" distance=1 freq=5 index=4304
  close match: "function" distance=1 freq=15 index=4302
  close match: "Function" distance=1 freq=10 index=1063
  close match: "function\226\128\153s" distance=1 freq=5 index=4307
  close match: "function" distance=2 freq=15 index=4302
  close match: "functions" distance=2 freq=15 index=4306
  close match: "function" distance=2 freq=15 index=4302
  close match: "Function" distance=2 freq=10 index=1063
  close match: "function" distance=2 freq=15 index=4302
  found: "value" freq=15 index=7722
  prefix: "value" freq=15 index=7722
  prefix: "valuerestriction" freq=0 index=7723
  close match: "valuerestriction" distance=1 freq=0 index=7723
  close match: "values" distance=1 freq=15 index=7724
  close match: "value" distance=1 freq=15 index=7722
  close match: "Value" distance=1 freq=8 index=2185
  close match: "glue" distance=2 freq=0 index=4388
  close match: "valid" distance=2 freq=14 index=7720
  close match: "Value" distance=2 freq=8 index=2185
  close match: "Blue" distance=2 freq=5 index=593
  close match: "values" distance=2 freq=15 index=7724
  found: "match" freq=15 index=5267
  prefix: "match" freq=15 index=5267
  prefix: "matched" freq=14 index=5268
  prefix: "matching" freq=15 index=5270
  prefix: "matches" freq=14 index=5269
  close match: "matched" distance=1 freq=14 index=5268
  close match: "matches" distance=1 freq=14 index=5269
  close match: "batch" distance=1 freq=11 index=2679
  close match: "Batch" distance=1 freq=0 index=566
  close match: "matchings" distance=1 freq=11 index=5271
  close match: "outch" distance=2 freq=10 index=5806
  close match: "much" distance=2 freq=13 index=5444
  close match: "batch" distance=2 freq=11 index=2679
  close match: "Batch" distance=2 freq=0 index=566
  close match: "match" distance=2 freq=15 index=5267

  $ cdict-tool query dict overload enum defensive coding
  not found: "overload"
  close match: "overloading" distance=1 freq=0 index=5830
  close match: "overlap" distance=2 freq=7 index=5827
  close match: "overlook" distance=2 freq=0 index=5831
  close match: "overlay" distance=2 freq=0 index=5829
  close match: "overhead" distance=2 freq=9 index=5826
  not found: "enum"
  close match: "enumerated" distance=1 freq=10 index=3886
  close match: "enumerate" distance=1 freq=13 index=3885
  close match: "vnum" distance=1 freq=12 index=7774
  close match: "num" distance=1 freq=14 index=5623
  close match: "enumeration" distance=1 freq=0 index=3887
  close match: "sum" distance=2 freq=14 index=7171
  close match: "end" distance=2 freq=15 index=3844
  close match: "nsm" distance=2 freq=7 index=5619
  close match: "Num" distance=2 freq=11 index=1553
  close match: "env" distance=2 freq=13 index=3888
  not found: "defensive"
  not found: "coding"
  close match: "coming" distance=1 freq=5 index=3093
  close match: "Coding" distance=1 freq=0 index=712
  close match: "moving" distance=2 freq=5 index=5439
  close match: "scoping" distance=2 freq=0 index=6721
  close match: "Coding" distance=2 freq=0 index=712
  close match: "owing" distance=2 freq=0 index=5843
  close match: "going" distance=2 freq=9 index=4394
  [4]

Prefix search:

  $ cdict-tool query dict typ
  found: "typ" freq=15 index=7524
  prefix: "typ" freq=15 index=7524
  prefix: "typing" freq=13 index=7544
  prefix: "typexr" freq=5 index=7541
  prefix: "typically" freq=14 index=7543
  prefix: "typexpr" freq=15 index=7540
  close match: "Typ" distance=1 freq=12 index=2113
  close match: "tmp" distance=1 freq=10 index=7398
  close match: "typ" distance=1 freq=15 index=7524
  close match: "top" distance=1 freq=15 index=7416
  close match: "typable" distance=1 freq=0 index=7525
  close match: "utop" distance=2 freq=0 index=7711
  close match: "up" distance=2 freq=15 index=7681
  close match: "pp" distance=2 freq=13 index=6056
  close match: "p" distance=2 freq=15 index=5847
  close match: "step" distance=2 freq=14 index=7058

Stats:

  $ cdict-tool stats words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Nodes: 4785
  Branch nodes: 2262
    With size:
    |  0: 1    |  2: 1287 |  3: 446  |  4: 209  |  5: 80   |  6: 49  
    |  7: 51   |  8: 24   |  9: 20   | 10: 19   | 11: 13   | 12: 13  
    | 13: 7    | 14: 6    | 15: 5    | 16: 7    | 17: 2    | 18: 3   
    | 19: 4    | 20: 1    | 21: 4    | 22: 3    | 23: 1    | 24: 3   
    | 25: 1    | 26: 1    | 27: 1    | 68: 1    |
    Transitions: 7597
      | Final: 2081 | Non-final: 5516 |
      With numbers length in bytes: |  1: 7597 |
    With numbers format: | None: 2249 | U8: 13   |
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
  Freq: 7984
    With value:
    |  0: 2611 |  5: 1156 |  7: 666  |  8: 453  |  9: 342  | 10: 281 
    | 11: 531  | 12: 490  | 13: 468  | 14: 492  | 15: 494  |
