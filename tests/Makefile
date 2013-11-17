CAMLP4=
OCAMLC_OPTS=-package pastry,oUnit,qcheck
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

TEST_MODULES=\
	key_tests.ml hexstring_tests.ml \
	routing_table_tests.ml node_tests.ml \
	leaf_set_tests.ml \
	key_props.ml

TEST_DEPS=../lib/pastry/pastry.cma ../lib/pastry/pastry.cmxa

.PHONY: all test

all: test

# Dependencies
include ../lib/Ocamlrules.mk.in
