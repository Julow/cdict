  $ cdict-tool build -o dict words.combined
  Parsing "words.combined"
  Generating a 7984 words dictionary.
  Done.

  $ ls -sh dict
  120K dict

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
  Transitions: 3105
    | Final: 971  | Non-final: 2134 |
    With numbers length in bytes: |  1: 2972 |  2: 133  |
    With char:
    |  0: 30   |  1: 50   |  2: 57   |  3: 49   |  4: 39   |  5: 30  
    |  6: 31   |  7: 31   |  8: 28   |  9: 21   |  ?: 1    |  A: 17  
    |  B: 8    |  C: 16   |  D: 11   |  E: 15   |  F: 9    |  G: 2   
    |  H: 4    |  I: 12   |  J: 1    |  K: 4    |  L: 13   |  M: 10  
    |  N: 11   |  O: 14   |  P: 11   |  Q: 2    |  R: 12   |  S: 12  
    |  T: 13   |  U: 9    |  V: 5    |  W: 3    |  X: 4    |  Y: 4   
    |  Z: 1    |  a: 178  |  b: 43   |  c: 112  |  d: 86   |  e: 346 
    |  f: 62   |  g: 55   |  h: 44   |  i: 277  |  j: 9    |  k: 28  
    |  l: 146  |  m: 91   |  n: 121  |  o: 105  |  p: 103  |  q: 14  
    |  r: 169  |  s: 143  |  t: 118  |  u: 91   |  v: 53   |  w: 40  
    |  x: 29   |  y: 34   |  z: 9    |  Ã: 5    |  Å: 1    |  á: 1   
    |  â: 1    |  ð: 1    |
