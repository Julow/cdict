  $ cdict-tool build -o dict ocaml_manual.txt
  Parsing "ocaml_manual.txt"
  Generating a 7984 words dictionary.
  Done.

  $ java -cp cdict_java.jar -Djava.library.path=$PWD juloo.cdict.tests/CdictJavaTests.java -- dict type module function value match
  found: type freq=15 index=7526
  prefix: typexpr freq=15 index=7540
  prefix: types freq=15 index=7537
  prefix: typeconstr freq=14 index=7530
  prefix: typename freq=0 index=7535
  prefix: typecheck freq=7 index=7527
  close match: Type distance=1 freq=14
  close match: typexpr distance=1 freq=15
  close match: typer distance=1 freq=0
  close match: type distance=1 freq=15
  close match: types distance=1 freq=15
  close match: style distance=2 freq=14
  close match: mytype distance=2 freq=9
  close match: Type distance=2 freq=14
  close match: type distance=2 freq=15
  close match: Style distance=2 freq=0
  found: module freq=15 index=5399
  prefix: moduleref freq=5 index=5403
  prefix: moduleexamples freq=10 index=5401
  prefix: modulename freq=5 index=5402
  prefix: modulealias freq=5 index=5400
  prefix: module freq=15 index=5399
  close match: modules distance=1 freq=15
  close match: moduleexamples distance=1 freq=10
  close match: mdule distance=1 freq=11
  close match: Module distance=1 freq=14
  close match: moduleref distance=1 freq=5
  close match: middle distance=2 freq=11
  close match: mobile distance=2 freq=0
  close match: mdule distance=2 freq=11
  close match: Module distance=2 freq=14
  close match: models distance=2 freq=9
  found: function freq=15 index=4302
  prefix: functions freq=15 index=4306
  prefix: functional freq=14 index=4303
  prefix: function freq=15 index=4302
  prefix: functionals freq=7 index=4305
  prefix: functionality freq=5 index=4304
  close match: functionals distance=1 freq=7
  close match: functionality distance=1 freq=5
  close match: function distance=1 freq=15
  close match: Function distance=1 freq=10
  close match: function?s distance=1 freq=5
  close match: function distance=2 freq=15
  close match: functions distance=2 freq=15
  close match: function distance=2 freq=15
  close match: Function distance=2 freq=10
  close match: function distance=2 freq=15
  found: value freq=15 index=7722
  prefix: value freq=15 index=7722
  prefix: valuerestriction freq=0 index=7723
  close match: valuerestriction distance=1 freq=0
  close match: values distance=1 freq=15
  close match: value distance=1 freq=15
  close match: Value distance=1 freq=8
  close match: glue distance=2 freq=0
  close match: valid distance=2 freq=14
  close match: Value distance=2 freq=8
  close match: Blue distance=2 freq=5
  close match: values distance=2 freq=15
  found: match freq=15 index=5267
  prefix: match freq=15 index=5267
  prefix: matched freq=14 index=5268
  prefix: matching freq=15 index=5270
  prefix: matches freq=14 index=5269
  close match: matched distance=1 freq=14
  close match: matches distance=1 freq=14
  close match: batch distance=1 freq=11
  close match: Batch distance=1 freq=0
  close match: matchings distance=1 freq=11
  close match: outch distance=2 freq=10
  close match: much distance=2 freq=13
  close match: batch distance=2 freq=11
  close match: Batch distance=2 freq=0
  close match: match distance=2 freq=15

  $ java -cp cdict_java.jar -Djava.library.path=$PWD juloo.cdict.tests/CdictJavaTests.java -- dict overload enum defensive coding
  not found: overload
  close match: overloading distance=1 freq=0
  close match: overlap distance=2 freq=7
  close match: overlook distance=2 freq=0
  close match: overlay distance=2 freq=0
  close match: overhead distance=2 freq=9
  not found: enum
  close match: enumerated distance=1 freq=10
  close match: enumerate distance=1 freq=13
  close match: vnum distance=1 freq=12
  close match: num distance=1 freq=14
  close match: enumeration distance=1 freq=0
  close match: sum distance=2 freq=14
  close match: end distance=2 freq=15
  close match: nsm distance=2 freq=7
  close match: Num distance=2 freq=11
  close match: env distance=2 freq=13
  not found: defensive
  not found: coding
  close match: coming distance=1 freq=5
  close match: Coding distance=1 freq=0
  close match: moving distance=2 freq=5
  close match: scoping distance=2 freq=0
  close match: Coding distance=2 freq=0
  close match: owing distance=2 freq=0
  close match: going distance=2 freq=9
