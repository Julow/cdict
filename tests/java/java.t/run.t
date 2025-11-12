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
  close match: Type distance=1 freq=14
  close match: hyperfine distance=1 freq=10
  close match: hyperref distance=1 freq=14
  close match: typable distance=1 freq=0
  close match: style distance=2 freq=14
  close match: supertype distance=2 freq=11
  close match: pending distance=2 freq=12
  close match: pessimization distance=2 freq=0
  close match: superscript distance=2 freq=5
  found: module freq=15 index=5399
  prefix: moduleref freq=5 index=5403
  prefix: moduleexamples freq=10 index=5401
  prefix: modulename freq=5 index=5402
  prefix: modulealias freq=5 index=5400
  prefix: module freq=15 index=5399
  close match: modular distance=1 freq=9
  close match: modulus distance=1 freq=7
  close match: Module1 distance=1 freq=7
  close match: Module distance=1 freq=14
  close match: moduleexamples distance=1 freq=10
  close match: names distance=2 freq=15
  close match: name distance=2 freq=15
  close match: Module1 distance=2 freq=7
  close match: Module distance=2 freq=14
  close match: naive distance=2 freq=9
  found: function freq=15 index=4302
  prefix: functions freq=15 index=4306
  prefix: functional freq=14 index=4303
  prefix: function freq=15 index=4302
  prefix: functionals freq=7 index=4305
  prefix: functionality freq=5 index=4304
  close match: Functional distance=1 freq=7
  close match: Function distance=1 freq=10
  close match: Functionals distance=1 freq=0
  close match: Function distance=1 freq=10
  close match: further distance=2 freq=13
  close match: functor distance=2 freq=15
  close match: Functionals distance=2 freq=0
  close match: Function distance=2 freq=10
  close match: functors distance=2 freq=14
  found: value freq=15 index=7722
  prefix: value freq=15 index=7722
  prefix: valuerestriction freq=0 index=7723
  close match: vanilla distance=1 freq=5
  close match: vanishes distance=1 freq=0
  close match: Value distance=1 freq=8
  close match: Value distance=1 freq=8
  close match: values distance=1 freq=15
  close match: improvement distance=2 freq=5
  close match: evaluating distance=2 freq=8
  close match: Value distance=2 freq=8
  close match: blue distance=2 freq=5
  close match: evaluation distance=2 freq=14
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
  close match: mixed distance=2 freq=12
  close match: batch distance=2 freq=11
  close match: Batch distance=2 freq=0
  close match: mix distance=2 freq=7

  $ java -cp cdict_java.jar -Djava.library.path=$PWD juloo.cdict.tests/CdictJavaTests.java -- dict overload enum defensive coding
  not found: overload
  close match: overloading distance=1 freq=0
  close match: overly distance=2 freq=5
  close match: overlook distance=2 freq=0
  close match: overrides distance=2 freq=7
  close match: overhead distance=2 freq=9
  close match: overridden distance=2 freq=8
  not found: enum
  close match: every distance=1 freq=13
  close match: ever distance=1 freq=8
  close match: numbers distance=1 freq=14
  close match: num distance=1 freq=14
  close match: everything distance=1 freq=10
  close match: vy distance=2 freq=7
  close match: unambiguously distance=2 freq=5
  close match: umask distance=2 freq=0
  close match: Jump distance=2 freq=0
  close match: unresponsive distance=2 freq=0
  not found: defensive
  not found: coding
  close match: coming distance=1 freq=5
  close match: Coding distance=1 freq=0
  close match: moving distance=2 freq=5
  close match: scoping distance=2 freq=0
  close match: Coding distance=2 freq=0
  close match: owing distance=2 freq=0
  close match: hash distance=2 freq=14
