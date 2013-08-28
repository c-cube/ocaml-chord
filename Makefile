
INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = libchord.cma libchord.cmxa
TARGETS_DOC = libchord.docdir/index.html
TARGETS_BIN = chat.native

OPTIONS = -use-ocamlfind -tag debug

all: lib bin doc

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB)

doc:
	ocamlbuild $(OPTIONS) $(TARGETS_DOC)

bin: lib
	ocamlbuild $(OPTIONS) $(TARGETS_BIN)

tests: lib
	ocamlbuild $(OPTIONS) -package oUnit -I . tests/run_tests.native

clean:
	ocamlbuild -clean

tags:
	otags *.ml *.mli

.PHONY: all clean tests tags

