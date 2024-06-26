(lang dune 3.0)
(using menhir 2.1)

(name encoding)

(generate_opam_files true)

(source
 (github formalsec/encoding))

(authors "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(maintainers "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(documentation "https://formalsec.github.io/encoding/encoding/index.html")

(license "GPL-3.0-only")

(package
 (name encoding)
 (synopsis "Smt encoding library")
 (description "An OCaml abstraction layer for constraint solvers.")
 (depends
   dune
   (ocaml (>= "4.14.0"))
   ocaml_intrinsics
   (z3 (and (>= "4.12.2") (< "4.13")))
   (menhir
    (and
    :build
    (>= 20220210)))
   (cmdliner (>= "1.2.0"))
   (zarith (>= "1.5"))
   (odoc :with-doc)
   (hc (>= "0.3"))
   (bisect_ppx (and :with-test (>= "2.5.0") :dev))))

(package
 (allow_empty)
 (name colibri2_mappings)
 (synopsis "Encoding mappings for the colibri2 Solver")
 (depopts colibri2)
 (depends encoding))
