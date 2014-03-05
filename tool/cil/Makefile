OCAMLMAKEFILE = OCamlMakefile
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFIND = ocamlfind
OCAMLLIBPATH = /usr/local/lib/ocaml /usr/local/lib/ocaml/cil
NCSUFFIX = .native
BCSUFFIX = .byte
PP=camlp4o pa_macro.cmo
export PP
OCAML_VERSION = $(shell ocamlc -version)
OCAML_MAJOR = $(shell ocamlc -version | cut -c1)
PPFLAGS =
ifeq ($(OCAML_MAJOR), 4)
	PPFLAGS = -DOCAML4
endif

SOURCES = \
	hash.ml inliner.ml oneReturn.ml \
	exprParser.mly exprLexer.mll tran.ml \
	main.ml
INCDIRS = /usr/local/lib/ocaml/cil /usr/local/lib/ocaml/$(OCAML_VERSION)/cil
LIBDIRS = /usr/local/lib/ocaml/cil /usr/local/lib/ocaml/$(OCAML_VERSION)/cil
MODULES = 
LIBS = unix nums str cil
OCAMLFLAGS = -g
LDFLAGS = -fexceptions
#CLIBS = stdc++ 
RESULT = tran

-include $(OCAMLMAKEFILE)
