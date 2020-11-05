MODULES= lambdaast interp ast convert
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 
PKGS=unix,oUnit
GIT_HASH=$(shell git log --pretty=format:'%h' -n 1)

default: build

build:
	$(OCAMLBUILD) $(OBJECTS) $(MAIN)

run:
	$(OCAMLBUILD) $(OBJECTS) $(MAIN) && ./$(MAIN)

zip:
	zip OScrabble-$(GIT_HASH).zip *.ml* _tags Makefile  *.txt *.md

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report