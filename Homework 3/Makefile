#####################################################################
# CS546 - http://www.csd.uoc.gr/~hy546/
#
# This file is part of homework 3
#

SRC=ast.mli ast.ml parser.mli lexer.ml parser.ml homework3.mli homework3.ml main.ml #tests3.ml
TESTS=$(shell ls *.490)
TESTOUT=$(TESTS:.490=.out)

all: $(SRC)
	ocamlc $(SRC) -o hw3 || true
	./hw3

tests: all $(TESTOUT)

parser.ml: parser.mly
	ocamlyacc -v parser.mly

parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	$(RM) -f *cmi *cmo a.out lexer.ml parser.ml parser.mli parser.output tests hw3

%.out: hw3
	./hw3 < $(@:%.out=%.490) | diff $@ -
