FLEX = flex
CC = gcc
CXX = g++
CXXFLAGS = -std=c++17

all: lexpp cpp

dev: lexpp dev_cpp

lexpp: scanner.l
	${FLEX} -o hw1.yy.cpp $?

cpp: hw1.yy.cpp
	${CXX} ${CXXFLAGS} $? -o scanner -lfl
	chmod +x scanner

dev_cpp: hw1.yy.cpp
	${CXX} ${CXXFLAGS} -DDEV $? -o scanner -lfl
	chmod +x scanner



.PHONY: clean

clean:
	rm -f hw1.yy.cpp scanner
