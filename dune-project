(lang dune 3.13)

(name cdict)

(using menhir 3.0)

(generate_opam_files true)

(package
 (name cdict)
 (synopsis "Compact dictionary partially written in C for spell checking."))

(package
 (name cdict-tool)
 (synopsis "Compiles dictionaries for the cdict library.")
 (depends cmdliner menhir cdict))

(package
 (name dfa)
 (synopsis "Construct a minimal DFA that recognizes a list of words.")
 (description
  "Implements a minimal deterministic finite-state acyclic automaton that recognizes a finite set of words. It works with a 8 bits alphabet and treat words as byte strings. It is based on the algorithm from the paper:\n\nIncremental Construction of Minimal Acyclic Finite-State Automata\nby Jan Daciuk, Stoyan Mihov, Bruce Watson, Richard Watson\nhttps://arxiv.org/pdf/cs/0007009"))
