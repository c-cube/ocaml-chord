
INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = libchord.cma libchord.cmxa
TARGETS_DOC = libchord.docdir/index.html

OPTIONS = -use-ocamlfind

all: lib doc

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB)

doc:
	ocamlbuild $(OPTIONS) $(TARGETS_DOC)

tests: lib
	ocamlbuild $(OPTIONS) -package oUnit -I . tests/run_tests.native

clean:
	ocamlbuild -clean

tags:
	otags *.ml *.mli

.PHONY: all clean tests tags

