prolog_interpreter:
	ocamlc -c types.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c MGU.ml
	ocamlc -c unifier_solve.ml
	ocamlc -o opl.exe types.cmo lexer.cmo parser.cmo MGU.cmo unifier_solve.cmo  opl.ml


