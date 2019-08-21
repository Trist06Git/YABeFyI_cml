build: dune
	dune build main.exe

run:
	dune exec ./main.exe test.b

clean:
	dune clean

clean_all:
	-rm -rf dist
	$(MAKE) clean

dist: build
	-rm -rf dist
	mkdir dist
	cp _build/default/main.exe dist/YABeFyI_cml.exe
	cp test.b dist

rebuild:
	$(MAKE) clean
	$(MAKE)
