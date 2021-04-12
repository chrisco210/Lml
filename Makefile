MODULES=  lambdaast pprint ast iast convertcps convert interp parse convertraw repl fileinterp colorprinter stdprinter
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
MAINJS=main_online.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 
PKGS=unix,oUnit,js_of_ocaml.ocamlbuild,js_of_ocaml,js_of_ocaml-ppx 
GIT_HASH=$(shell git log --pretty=format:'%h' -n 1)

default: run

js:
	$(OCAMLBUILD) $(OBJECTS)  $(MAINJS) 
	js_of_ocaml $(MAINJS)

build:
	$(OCAMLBUILD) $(OBJECTS) $(MAINJS)

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