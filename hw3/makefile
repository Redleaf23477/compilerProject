FLEX = flex
BYACC = byacc
YACC = yacc
CC = gcc
CXX = g++
CXXFLAGS = -std=c++17
DEBUG = 
LDFLAGS = -lfl

all: ytab scanner codegen

ytab: parser.y
	${BYACC} -d parser.y

yystype: yystype.cpp yystype.h
	${CXX} ${CXXFLAGS} ${DEBUG} -o $@.o $<

conflict: parser.y
	${YACC} -v $^

scanner: scanner.l y.tab.h yystype.h
	${FLEX} -o lex.yy.cpp $<

codegen: yystype.o y.tab.c lex.yy.cpp
	${CXX} ${CXXFLAGS} ${DEBUG} -o $@ $^ ${LDFLAGS}
	chmod +x $@

.PHONY: clean

clean:
	rm -f lex.yy.cpp y.tab.h y.tab.c y.output yystype.o parser
