CAMLP4=
OCAMLC_OPTS=-package pastry,oUnit
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=
NON_LIB_MODULES=key_tests.ml

BYTE_TARGETS=$(NON_LIB_MODULES:%.ml=%.byte)
NATIVE_TARGETS=$(NON_LIB_MODULES:%.ml=%.native)

.PHONY: all install test

all: byte-code native-code

key_tests.native: key_tests.cmx

key_tests.byte: key_tests.cmo

test: byte-code native-code
	./base_tests.native
	./base_tests.byte

# Dependencies
include ../lib/Ocamlrules.mk.in
