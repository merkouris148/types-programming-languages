#####################################################################
# CS546 - http://www.csd.uoc.gr/~hy546/
#
# This file is part of homework 2
#
ifndef HW
HW = .
endif


SRC=ast.ml \
  parser.mli \
  lexer.ml \
  parser.ml \
  homework2.mli \
  homework2.ml \
  main.ml \
  #tests2.ml


all: $(SRC)
	ocamlc $(SRC) -o test || true
	./test

parser.ml: parser.mly
	ocamlyacc -v parser.mly

parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	$(RM) -f *cmi *cmo test a.out lexer.ml parser.ml parser.mli parser.output
