YABeFyI_cml.exe: *.ml
	ocamlc file_io.ml mon_stack.ml br_check.ml mon_maths.ml machine.ml main.ml -o YABeFyI_cml.exe

run: YABeFyI_cml.exe
	./YABeFyI_cml.exe test.b

clean:
	-rm YABeFyI_cml.exe
	-rm mon_maths.cmi machine.cmi main.cmi br_check.cmi mon_stack.cmi file_io.cmi
	-rm mon_maths.cmo machine.cmo main.cmo br_check.cmo mon_stack.cmo file_io.cmo

clean_all:
	-rm -rf dist
	$(MAKE) clean

dist: YABeFyI_cml.exe
	-rm -rf dist
	mkdir dist
	ocamlc -custom file_io.ml mon_stack.ml br_check.ml mon_maths.ml machine.ml main.ml -o dist/YABeFyI_cml.exe
	cp test.b dist


rebuild:
	$(MAKE) clean
	$(MAKE)
