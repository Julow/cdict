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
