
INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = kademlia.cma kademlia.cmxa
TARGETS_DOC = kademlia.docdir/index.html

OPTIONS = -use-ocamlfind -packages lwt,lwt.unix

all: lib

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB) $(TARGETS_DOC)

doc:
	ocamlbuild $(OPTIONS) $(TARGETS_DOC)

tests: lib
	ocamlbuild $(OPTIONS) -package oUnit -I . tests/run_tests.native

clean:
	ocamlbuild -clean

tags:
	otags *.ml *.mli

.PHONY: all clean tests tags

