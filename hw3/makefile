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

judge: basic_test arithmetic_test pointer_test jump_test

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
	@echo "=================="
	@echo "Basic/0.c"
	@echo "=================="
	./codegen <Testcase/Basic/0.c
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@ | tee out
	diff out Testcase/Basic/0.ans

arithmetic_test: codegen
	@echo "=================="
	@echo "ArithmeticExpression/0.c"
	@echo "=================="
	./codegen <Testcase/ArithmeticExpression/0.c
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@ | tee out
	diff out Testcase/ArithmeticExpression/0.ans
	@echo "=================="
	@echo "ArithmeticExpression/1.c"
	@echo "=================="
	./codegen <Testcase/ArithmeticExpression/1.c
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@ | tee out
	diff out Testcase/ArithmeticExpression/1.ans

pointer_test: codegen
	@echo "=================="
	@echo "Pointer/0.c"
	@echo "=================="
	./codegen <Testcase/Pointer/0.c
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@ | tee out
	diff out Testcase/Pointer/0.ans
	@echo "=================="
	@echo "Pointer/1.c"
	@echo "=================="
	./codegen <Testcase/Pointer/1.c
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@ | tee out
	diff out Testcase/Pointer/1.ans

jump_test: codegen
	@echo "=================="
	@echo "Jump/0.c"
	@echo "=================="
	./codegen <Testcase/Jump/0.c
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@ | tee out
	diff out Testcase/Jump/0.ans
	@echo "=================="
	@echo "Jump/1.c"
	@echo "=================="
	./codegen <Testcase/Jump/1.c
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@ | tee out
	diff out Testcase/Jump/1.ans

test:
	${RISCV_CXX} -o codegen.o -c codegen.S
	${RISCV_CXX} -o $@ ${ARDUINO_MAIN} codegen.o
	${SPIKE_PK} ./$@
	rm -f ./$@
	

.PHONY: clean

clean:
	rm -f lex.yy.cpp y.tab.h y.tab.c y.output yystype.o parser codegen ast.txt codegen.S codegen.o basic_test out test
