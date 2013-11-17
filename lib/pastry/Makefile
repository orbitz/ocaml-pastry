CAMLP4=
PACK=pastry
OCAMLC_OPTS=-package core,sexplib.syntax,comparelib.syntax -syntax camlp4o
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=hexstring.ml key.ml node.ml routing_table.ml leaf_set.ml
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
