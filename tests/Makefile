CAMLP4=
OCAMLC_OPTS=-package pastry,oUnit,qcheck,test_lib
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

TEST_MODULES=\
	key_tests.ml hexstring_tests.ml \
	routing_table_tests.ml node_tests.ml \
	leaf_set_tests.ml \
	key_props.ml leaf_set_props.ml \
	routing_table_props.ml router_props.ml

TEST_DEPS=../lib/pastry/pastry.cma ../lib/pastry/pastry.cmxa \
	test_lib ../lib/test_lib/test_lib.cma ../lib/test_lib/test_lib.cmxa

.PHONY: all test test_lib

all: test

# A little hacky, but all libs belong in ../lib but we only want to build
# test_lib when we are actually testing
test_lib:
	$(MAKE) -C ../lib/test_lib

test_lib_clean:
	$(MAKE) -C ../lib/test_lib clean

clean: test_lib_clean

# Dependencies
include ../lib/Ocamlrules.mk.in
