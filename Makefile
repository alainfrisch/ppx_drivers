all: ppx_driver.exe ppx_driver.opt.exe

FLAGS=-w -40 -package compiler-libs.common,findlib,dynlink


ppx_driver.exe: ppx_driver.ml
	ocamlfind ocamlc -c $(FLAGS) ppx_driver.ml
	ocamlfind ocamlc -linkall -o ppx_driver.exe -linkpkg $(FLAGS) ppx_driver.cmo

ppx_driver.opt.exe: ppx_driver.ml
	ocamlfind ocamlopt -c $(FLAGS) ppx_driver.ml
	ocamlfind ocamlopt -linkall -o ppx_driver.opt.exe -linkpkg $(FLAGS) ppx_driver.cmx

clean:
	rm -f *~ *.cm* *.exe *.o *.obj *.a *.lib

test: ppx_driver.exe
	ocamlfind ocamlc -package sedlex -c -ppx "./ppx_driver.exe -v" test_sedlex.ml

test.opt: ppx_driver.opt.exe
	ocamlfind ocamlopt -package sedlex -c -ppx "./ppx_driver.opt.exe -v" test_sedlex.ml
