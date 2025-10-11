  $ cdict-tool build -o dict words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  144K dict

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
  Branch nodes: 3888
    With leaf: 2061
    With 'next' nodes:
    |  1: 1957 |  2: 1623 |  3: 267  |  4: 25   |  5: 9    |  6: 4   
    |  7: 3    |
    Ranges: 6194:
      |  1: 4333 |  2: 1262 |  3: 314  |  4: 86   |  5: 58   |  6: 34  
      |  7: 27   |  8: 7    |  9: 15   | 10: 15   | 11: 8    | 12: 5   
      | 13: 8    | 14: 1    | 15: 6    | 16: 3    | 17: 4    | 18: 1   
      | 19: 2    | 20: 1    | 21: 2    | 26: 1    | 27: 1    |
  Prefix nodes: 6637
    Followed by:
    | Branches: 1403 | Leaf: 3844 | Prefix: 1390 |
    With size:
    |  1: 2464 |  2: 1749 |  3: 2424 |
