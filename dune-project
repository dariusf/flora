(lang dune 1.10)
(using fmt 1.0)
;(implicit_transitive_deps false)

(generate_opam_files true)

(license MIT)
(authors "Darius Foo <darius.foo.tw@gmail.com>")
(source (github dariusf/flora))

(package
 (name flora)
 (depends
  (ocaml (< 4.08))
   (ppx_deriving (= 4.3))
   (containers (= 2.5))
   (lambda-term (= 2.0.1))
   (teash (= 0.1.0))
   (lens (= 1.2.2)))
 (synopsis "A practical structural editor"))
