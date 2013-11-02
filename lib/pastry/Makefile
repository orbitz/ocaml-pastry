CAMLP4=
PACK=pastry
OCAMLC_OPTS=-package core
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=key.ml
NON_LIB_MODULES=

BYTE_TARGETS=pastry.cma
NATIVE_TARGETS=pastry.cmxa

.PHONY: all test

all: native-code byte-code

install: all
	ocamlfind install pastry META pastry.cmxa pastry.cma pastry.a *.cmx *.cmi *.mli

test:

# Dependencies
include ../Ocamlrules.mk.in
