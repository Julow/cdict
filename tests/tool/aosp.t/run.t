  $ cdict-tool build -o dict words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  112K dict

  $ cdict-tool query dict type module function value match
  found: "type" freq=1
  found: "module" freq=2
  found: "function" freq=1
  found: "value" freq=3
  found: "match" freq=4

  $ cdict-tool query dict overload enum defensive coding
  not found: "overload"
  not found: "enum"
  not found: "defensive"
  not found: "coding"
  [4]

  $ cdict-tool stats words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Nodes: 4960
  Branch nodes: 742
    With 'next' nodes:
    |  1: 649  |  2: 46   |  3: 26   |  4: 9    |  5: 5    |  6: 4   
    |  7: 3    |
    Ranges: 925:
      |  0: 1    |  1: 72   |  2: 533  |  3: 122  |  4: 38   |  5: 32  
      |  6: 24   |  7: 23   |  8: 7    |  9: 15   | 10: 15   | 11: 8   
      | 12: 5    | 13: 8    | 14: 1    | 15: 6    | 16: 3    | 17: 4   
      | 18: 1    | 19: 2    | 20: 1    | 21: 2    | 26: 1    | 27: 1    |
  Prefix nodes: 2698
    Followed by:
    | Branches: 1192 | Btree: 474  | Prefix: 1032 |
    With size:
    |  1: 578  |  2: 547  |  3: 530  |  4: 1043 |
  Btree nodes: 1520
    With size:
    |  2: 802  |  3: 355  |  4: 189  |  5: 65   |  6: 44   |  7: 42  
    |  8: 23   |
  Leaves: 7984
