FLEX = flex
BYACC = byacc
YACC = yacc
CC = gcc
CXX = g++
CXXFLAGS = -std=c++17
LDFLAGS = -lfl

all: ytab scanner parser

ytab: parser.y
	${BYACC} -d parser.y

scanner: y.tab.h scanner.l
	${FLEX} -o lex.yy.cpp scanner.l

parser: y.tab.c lex.yy.cpp
	${CXX} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}
	chmod +x $@

.PHONY: clean

clean:
	rm -f lex.yy.cpp y.tab.h y.tab.c
