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

test:
	./codegen < Testcase/Function/0.c
	riscv32-unknown-elf-gcc main.c codegen.S
	qemu-riscv32 a.out
	./codegen < Testcase/Function/1.c
	riscv32-unknown-elf-gcc main.c codegen.S
	qemu-riscv32 a.out

.PHONY: clean

clean:
	rm -f lex.yy.cpp y.tab.h y.tab.c y.output yystype.o parser codegen ast.txt codegen.S codegen.o a.out