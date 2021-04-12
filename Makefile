MODULES=  lambdaast pprint ast iast convertcps convert interp parse convertraw repl fileinterp colorprinter
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 
PKGS=unix,oUnit,js_of_ocaml
GIT_HASH=$(shell git log --pretty=format:'%h' -n 1)

default: run

js:
	$(OCAMLBUILD) $(OBJECTS)  $(MAIN) 

build:
	$(OCAMLBUILD) $(OBJECTS) $(MAIN)

run:
	$(OCAMLBUILD) $(OBJECTS) $(MAIN) && ./$(MAIN)
test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

zip:
	zip LML-$(GIT_HASH).zip *.ml* _tags Makefile  *.txt *.md examples/*.lml

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.byte