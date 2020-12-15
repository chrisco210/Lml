MODULES= lambdaast interp ast convert pprint parse church convertcps iast
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 
PKGS=unix,oUnit
GIT_HASH=$(shell git log --pretty=format:'%h' -n 1)

default: run

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