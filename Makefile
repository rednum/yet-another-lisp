STR_LIB_PATH = /usr/lib/ocaml/str.cma

all: core.cmo evaluator.cmo interpreter.cmo repl.ml
	ocamlc core.cmo $(STR_LIB_PATH) evaluator.cmo interpreter.cmo repl.ml -o repl

core.cmo: core.ml core.mli
	ocamlc -c core.mli
	ocamlc -w up -c core.ml

evaluator.cmo: evaluator.ml evaluator.mli
	ocamlc -c evaluator.mli
	ocamlc -c -w up evaluator.ml

interpreter.cmo: interpreter.ml interpreter.mli
	ocamlc -c interpreter.mli
	ocamlc -w up -c interpreter.ml

clean:
	rm *.cmo *.cmi
	rm repl
