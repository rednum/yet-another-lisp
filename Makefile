STR_LIB_PATH = /usr/lib/ocaml/str.cma

all: core.cmo library.cmo evaluator.cmo interpreter.cmo repl.ml
	ocamlc core.cmo $(STR_LIB_PATH) library.cmo evaluator.cmo interpreter.cmo repl.ml -o repl

core.cmo: core.ml core.mli
	ocamlc -c core.mli
	ocamlc -w up -c core.ml

library.cmo: library.ml library.mli
	ocamlc -c library.mli
	ocamlc -w up -c library.ml

evaluator.cmo: evaluator.ml evaluator.mli
	ocamlc -c evaluator.mli
	ocamlc -c -w up evaluator.ml

interpreter.cmo: interpreter.ml interpreter.mli
	ocamlc -c interpreter.mli
	ocamlc -w up -c interpreter.ml

clean:
	rm -rf *.cmo *.cmi
	rm -rf repl
