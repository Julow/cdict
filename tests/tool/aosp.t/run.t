  $ cdict-tool build -o dict words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  136K dict

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

  $ cdict-tool stats words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Nodes: 16448
  Leaf nodes: 5923
  Branch nodes: 2050
    With leaf: 1348
    With 'next' nodes:
    |  1: 1957 |  2: 46   |  3: 26   |  4: 9    |  5: 5    |  6: 4   
    |  7: 3    |
    Ranges: 2233:
      |  1: 1171 |  2: 737  |  3: 128  |  4: 38   |  5: 32   |  6: 24  
      |  7: 23   |  8: 7    |  9: 15   | 10: 15   | 11: 8    | 12: 5   
      | 13: 8    | 14: 1    | 15: 6    | 16: 3    | 17: 4    | 18: 1   
      | 19: 2    | 20: 1    | 21: 2    | 26: 1    | 27: 1    |
  Prefix nodes: 6637
    Followed by:
    | Branches: 915  | Btree: 488  | Leaf: 3844 | Prefix: 1390 |
    With size:
    |  1: 2464 |  2: 1749 |  3: 2424 |
  Btree nodes: 1838
    With leaf: 713
    With size:
    |  2: 1070 |  3: 403  |  4: 191  |  5: 65   |  6: 44   |  7: 42  
    |  8: 23   |
