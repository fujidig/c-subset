ocamllex lexer.mll
ocamlyacc parser.mly

ocamlc -c ast.mli
ocamlc -c ast.ml
ocamlc -c nums.cma parser.mli
ocamlc -c nums.cma lexer.ml
ocamlc -c nums.cma parser.ml
ocamlc -c nums.cma eval.ml
ocamlc -c calc.ml
ocamlc -o calc nums.cma ast.cmo lexer.cmo parser.cmo eval.cmo calc.cmo