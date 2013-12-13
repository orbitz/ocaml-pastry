CAMLP4=
OCAMLC_OPTS=-package core,qcheck,pastry
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

EXTERNAL_DEPS=../pastry/pastry.cmxa ../pastry/pastry.cmo

LIB_MODULES=test_lib.ml
NON_LIB_MODULES=

BYTE_TARGETS=test_lib.cma
NATIVE_TARGETS=test_lib.cmxa

.PHONY: all test

all: native-code byte-code

# Dependencies
include ../Ocamlrules.mk.in
