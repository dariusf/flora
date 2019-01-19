
all:
	dune exec src/app/main.exe

doc:
	dune build @doc
	open _build/default/_doc/_html/index.html

fmt:
	dune build @fmt --auto-promote

repl:
	dune utop src/lib
