FLEX = flex
CC = gcc
CXX = g++
CXXFLAGS = -std=c++17

all: lexpp cpp

lex: hw1.l
	${FLEX} -o hw1.yy.c $?

c: hw1.yy.c
	${CC} $? -o hw1 -lfl

lexpp: hw1.l
	${FLEX} -o hw1.yy.cpp $?

cpp: hw1.yy.cpp
	${CXX} ${CXXFLAGS} $? -o hw1 -lfl


.PHONY: clean

clean:
	rm -f hw1.l hw1.yy.c hw1
