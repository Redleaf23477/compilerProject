FLEX = flex
BYACC = byacc
YACC = yacc
CC = gcc
CXX = g++
CXXFLAGS = -std=c++17
DEBUG = -g -fsanitize=address
LDFLAGS = -lfl

RISCV_CXX = riscv64-unknown-elf-gcc
SPIKE_PK = spike pk
ARDUINO_MAIN = Spike/assembly/main.c

all: ytab yystype scanner codegen

ytab: parser.y
	${BYACC} -d parser.y

yystype: yystype.cpp yystype.h
	${CXX} -c ${CXXFLAGS} ${DEBUG} -o $@.o $<

conflict: parser.y
	${YACC} -v $^

scanner: scanner.l y.tab.h yystype.h
	${FLEX} -o lex.yy.cpp $<

codegen: yystype.o y.tab.c lex.yy.cpp
	${CXX} ${CXXFLAGS} ${DEBUG} -o $@ $^ ${LDFLAGS}
	chmod +x $@

basic_test: codegen
	./codegen <Testcase/Basic/0.c
	@echo "================"
	@echo "AST"
	@echo "================"
	@cat ast.txt
	@echo "================"
	@echo "codegen"
	@echo "================"
	@cat codegen.S
	@echo "================"
	@echo "compile & run"
	@echo "================"
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@ | tee out
	diff out Testcase/Basic/0.ans

test:
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@
	rm -f ./$@
	

.PHONY: clean

clean:
	rm -f lex.yy.cpp y.tab.h y.tab.c y.output yystype.o parser codegen ast.txt codegen.S codegen.o basic_test out test
