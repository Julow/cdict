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
  close match: "Type" distance=1 freq=14 index=2115
  close match: "Type" distance=1 freq=14 index=2115
  close match: "hyperfine" distance=1 freq=10 index=4549
  close match: "hyperref" distance=1 freq=14 index=4550
  close match: "typable" distance=1 freq=0 index=7525
  close match: "style" distance=2 freq=14 index=7108
  close match: "supertype" distance=2 freq=11 index=7186
  close match: "pending" distance=2 freq=12 index=5931
  close match: "pessimization" distance=2 freq=0 index=5960
  close match: "superscript" distance=2 freq=5 index=7184
  found: "module" freq=15 index=5399
  prefix: "moduleref" freq=5 index=5403
  prefix: "moduleexamples" freq=10 index=5401
  prefix: "modulename" freq=5 index=5402
  prefix: "modulealias" freq=5 index=5400
  prefix: "module" freq=15 index=5399
  close match: "modular" distance=1 freq=9 index=5398
  close match: "modulus" distance=1 freq=7 index=5407
  close match: "Module1" distance=1 freq=7 index=1467
  close match: "Module" distance=1 freq=14 index=1466
  close match: "moduleexamples" distance=1 freq=10 index=5401
  close match: "names" distance=2 freq=15 index=5498
  close match: "name" distance=2 freq=15 index=5496
  close match: "Module1" distance=2 freq=7 index=1467
  close match: "Module" distance=2 freq=14 index=1466
  close match: "naive" distance=2 freq=9 index=5494
  found: "function" freq=15 index=4302
  prefix: "functions" freq=15 index=4306
  prefix: "functional" freq=14 index=4303
  prefix: "function" freq=15 index=4302
  prefix: "functionals" freq=7 index=4305
  prefix: "functionality" freq=5 index=4304
  close match: "Functional" distance=1 freq=7 index=1064
  close match: "Function" distance=1 freq=10 index=1063
  close match: "Functionals" distance=1 freq=0 index=1065
  close match: "Function" distance=1 freq=10 index=1063
  close match: "further" distance=2 freq=13 index=4311
  close match: "functor" distance=2 freq=15 index=4308
  close match: "Functionals" distance=2 freq=0 index=1065
  close match: "Function" distance=2 freq=10 index=1063
  close match: "functors" distance=2 freq=14 index=4309
  found: "value" freq=15 index=7722
  prefix: "value" freq=15 index=7722
  prefix: "valuerestriction" freq=0 index=7723
  close match: "vanilla" distance=1 freq=5 index=7725
  close match: "vanishes" distance=1 freq=0 index=7726
  close match: "Value" distance=1 freq=8 index=2185
  close match: "Value" distance=1 freq=8 index=2185
  close match: "values" distance=1 freq=15 index=7724
  close match: "improvement" distance=2 freq=5 index=4630
  close match: "evaluating" distance=2 freq=8 index=3940
  close match: "Value" distance=2 freq=8 index=2185
  close match: "blue" distance=2 freq=5 index=2756
  close match: "evaluation" distance=2 freq=14 index=3941
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
  close match: "mixed" distance=2 freq=12 index=5363
  close match: "batch" distance=2 freq=11 index=2679
  close match: "Batch" distance=2 freq=0 index=566
  close match: "mix" distance=2 freq=7 index=5362

  $ cdict-tool query dict overload enum defensive coding
  not found: "overload"
  close match: "overloading" distance=1 freq=0 index=5830
  close match: "overly" distance=2 freq=5 index=5832
  close match: "overlook" distance=2 freq=0 index=5831
  close match: "overrides" distance=2 freq=7 index=5835
  close match: "overhead" distance=2 freq=9 index=5826
  close match: "overridden" distance=2 freq=8 index=5833
  not found: "enum"
  close match: "every" distance=1 freq=13 index=3948
  close match: "ever" distance=1 freq=8 index=3947
  close match: "numbers" distance=1 freq=14 index=5629
  close match: "num" distance=1 freq=14 index=5623
  close match: "everything" distance=1 freq=10 index=3949
  close match: "vy" distance=2 freq=7 index=7785
  close match: "unambiguously" distance=2 freq=5 index=7568
  close match: "umask" distance=2 freq=0 index=7563
  close match: "Jump" distance=2 freq=0 index=1281
  close match: "unresponsive" distance=2 freq=0 index=7654
  not found: "defensive"
  not found: "coding"
  close match: "coming" distance=1 freq=5 index=3093
  close match: "Coding" distance=1 freq=0 index=712
  close match: "moving" distance=2 freq=5 index=5439
  close match: "scoping" distance=2 freq=0 index=6721
  close match: "Coding" distance=2 freq=0 index=712
  close match: "owing" distance=2 freq=0 index=5843
  close match: "hash" distance=2 freq=14 index=4463
  [4]

Prefix search:

  $ cdict-tool query dict typ
  found: "typ" freq=15 index=7524
  prefix: "typ" freq=15 index=7524
  prefix: "typing" freq=13 index=7544
  prefix: "typexr" freq=5 index=7541
  prefix: "typically" freq=14 index=7543
  prefix: "typexpr" freq=15 index=7540
  close match: "mypoint" distance=1 freq=7 index=5480
  close match: "myprog" distance=1 freq=13 index=5481
  close match: "hyperref" distance=1 freq=14 index=4550
  close match: "hyphen" distance=1 freq=0 index=4551
  close match: "Types" distance=1 freq=0 index=2116
  close match: "utop" distance=2 freq=0 index=7711
  close match: "uppercase" distance=2 freq=13 index=7688
  close match: "powers" distance=2 freq=0 index=6055
  close match: "p" distance=2 freq=15 index=5847
  close match: "upper" distance=2 freq=11 index=7687

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
    With numbers: | 8 bits: 13   | None: 2249 |
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
