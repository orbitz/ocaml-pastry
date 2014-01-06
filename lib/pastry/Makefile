CAMLP4=
PACK=pastry

PACKAGES=async,async_unix,core,sexplib.syntax,comparelib.syntax,gen_server

OCAMLDEP_OPTS=-package $(PACKAGES) -syntax camlp4o
OCAMLC_OPTS=$(OCAMLDEP_OPTS) -w '@f@p@u@s@40'
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES= \
	hexstring.ml \
	key.ml node.ml \
	routing_table.ml leaf_set.ml router.ml \
	msg.ml listen_loop.ml \
	join_protocol.ml app.ml

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
