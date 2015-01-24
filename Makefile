YFLAGS = -v 
RESULT  = min-caml
LIBS = unix
SOURCES = m.ml s.ml id.ml type.ml pos.mli pos.ml syntax.mli syntax.ml \
	  lexer.mll parser.mly  typing.mli typing.ml \
          kNormal.mli kNormal.ml alpha.mli alpha.ml cse.mli cse.ml beta.mli beta.ml \
	  assoc.mli assoc.ml inline.mli inline.ml tupleArgExpand.mli tupleArgExpand.ml  \
	  constFold.mli constFold.ml elim.mli elim.ml closure.mli closure.ml \
	  asm.mli asm.ml  virtual.mli virtual.ml simm.mli simm.ml \
	  regAlloc.mli  regAlloc.ml emit.mli emit.ml main.mli main.ml \
	  float.c
all: native-code

clean_dbg_info: 
	rm *_dbg.txt
include OCamlMakefile
