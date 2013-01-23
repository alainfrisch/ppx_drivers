all: ppx_driver.exe ppx_driver.opt.exe

ppx_driver.exe: ppx_driver.ml
	ocamlc -o ppx_driver.exe -I +compiler-libs ocamlcommon.cma dynlink.cma ppx_driver.ml

ppx_driver.opt.exe: ppx_driver.ml
	ocamlopt -o ppx_driver.exe -I +compiler-libs ocamlcommon.cmxa dynlink.cmxa ppx_driver.ml

clean:
	rm -f *~ *.cm* *.exe *.o *.obj *.a *.lib
