RESULT  = min-caml
SOURCES = m.ml s.ml id.ml type.ml pos.ml lexer.mll parser.mly syntax.ml typing.ml    \
          kNormal.ml alpha.ml beta.ml assoc.ml inline.ml constFold.ml elim.ml \
          closure.ml asm.ml virtual.ml simm.ml regAlloc.ml emit.ml main.ml \
	  float.c
all: native-code

include OCamlMakefile
