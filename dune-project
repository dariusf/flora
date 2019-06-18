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
   ppx_deriving
   containers
   containers.data
   lambda-term
   teash
   lens)
 (synopsis "A practical structural editor"))
