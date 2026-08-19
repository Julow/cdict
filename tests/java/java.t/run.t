  $ cdict-tool build -o dict main:ocaml_manual.txt
  Parsing "ocaml_manual.txt"
  Built dictionary "main" (7984 words)
  Done.

  $ java -cp cdict_java.jar -Djava.library.path=$PWD juloo.cdict.tests/CdictJavaTests.java -- dict types
  found: types freq=9 index=7537
  prefix: types freq=9 index=7537
  prefix: typeset freq=0 index=7538
  close match: typed distance=1 freq=3
  close match: Types distance=1 freq=0
  close match: typer distance=1 freq=0
  close match: typeset distance=1 freq=0
  close match: type distance=2 freq=12
  close match: types distance=2 freq=9
  close match: types distance=2 freq=9
  close match: types distance=2 freq=9
  close match: types distance=2 freq=9
  $ java -cp cdict_java.jar -Djava.library.path=$PWD juloo.cdict.tests/CdictJavaTests.java -- dict type module function value match
  found: type freq=12 index=7526
  prefix: type freq=12 index=7526
  prefix: types freq=9 index=7537
  prefix: typexpr freq=9 index=7540
  prefix: typeconstr freq=6 index=7530
  prefix: typed freq=3 index=7531
  close match: types distance=1 freq=9
  close match: typexpr distance=1 freq=9
  close match: Type distance=1 freq=6
  close match: typeconstr distance=1 freq=6
  close match: typed distance=1 freq=3
  close match: the distance=2 freq=15
  close match: the distance=2 freq=15
  close match: type distance=2 freq=12
  close match: type distance=2 freq=12
  close match: type distance=2 freq=12
  found: module freq=11 index=5399
  prefix: module freq=11 index=5399
  prefix: modules freq=8 index=5404
  prefix: moduleexamples freq=3 index=5401
  prefix: modulealias freq=1 index=5400
  prefix: modulename freq=1 index=5402
  close match: modules distance=1 freq=8
  close match: Module distance=1 freq=5
  close match: modulo distance=1 freq=4
  close match: mdule distance=1 freq=3
  close match: moduleexamples distance=1 freq=3
  close match: module distance=2 freq=11
  close match: module distance=2 freq=11
  close match: module distance=2 freq=11
  close match: module distance=2 freq=11
  close match: module distance=2 freq=11
  found: function freq=11 index=4302
  prefix: function freq=11 index=4302
  prefix: functions freq=9 index=4306
  prefix: functional freq=6 index=4303
  prefix: functionality freq=1 index=4304
  prefix: functionals freq=1 index=4305
  close match: functions distance=1 freq=9
  close match: functional distance=1 freq=6
  close match: Function distance=1 freq=3
  close match: functionality distance=1 freq=1
  close match: functionals distance=1 freq=1
  close match: function distance=2 freq=11
  close match: function distance=2 freq=11
  close match: function distance=2 freq=11
  close match: function distance=2 freq=11
  close match: function distance=2 freq=11
  found: value freq=11 index=7722
  prefix: value freq=11 index=7722
  prefix: values freq=9 index=7724
  prefix: valuerestriction freq=0 index=7723
  close match: values distance=1 freq=9
  close match: Value distance=1 freq=2
  close match: valuerestriction distance=1 freq=0
  close match: value distance=2 freq=11
  close match: value distance=2 freq=11
  close match: value distance=2 freq=11
  close match: value distance=2 freq=11
  close match: values distance=2 freq=9
  found: match freq=9 index=5267
  prefix: match freq=9 index=5267
  prefix: matching freq=7 index=5270
  prefix: matches freq=6 index=5269
  prefix: matched freq=5 index=5268
  prefix: matchings freq=3 index=5271
  close match: matching distance=1 freq=7
  close match: matches distance=1 freq=6
  close match: Match distance=1 freq=5
  close match: matched distance=1 freq=5
  close match: batch distance=1 freq=3
  close match: match distance=2 freq=9
  close match: match distance=2 freq=9
  close match: match distance=2 freq=9
  close match: match distance=2 freq=9
  close match: path distance=2 freq=8

  $ java -cp cdict_java.jar -Djava.library.path=$PWD juloo.cdict.tests/CdictJavaTests.java -- dict overload enum defensive coding
  not found: overload
  prefix: overloading freq=0 index=5830
  close match: overloading distance=1 freq=0
  close match: overhead distance=2 freq=2
  close match: overlap distance=2 freq=1
  close match: overlay distance=2 freq=0
  close match: overlook distance=2 freq=0
  not found: enum
  prefix: enumerate freq=5 index=3885
  prefix: enumerated freq=3 index=3886
  prefix: enumeration freq=0 index=3887
  close match: num distance=1 freq=6
  close match: enumerate distance=1 freq=5
  close match: vnum distance=1 freq=4
  close match: enumerated distance=1 freq=3
  close match: enumeration distance=1 freq=0
  close match: end distance=2 freq=12
  close match: em distance=2 freq=8
  close match: number distance=2 freq=8
  close match: num distance=2 freq=6
  close match: numbers distance=2 freq=6
  not found: defensive
  not found: coding
  close match: coming distance=1 freq=1
  close match: Coding distance=1 freq=0
  close match: encoding distance=2 freq=5
  close match: adding distance=2 freq=4
  close match: copying distance=2 freq=4
  close match: copying distance=2 freq=4
  close match: ending distance=2 freq=4
