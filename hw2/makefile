# Define the compiler and flags
CC = gcc
FLEX = flex
BISON = yacc
CFLAGS = -lfl

# Define the files
LEX_FILE = scanner.l
YACC_FILE = parser.y
LEX_OUTPUT = lex.yy.c
YACC_OUTPUT = y.tab.c
YACC_HEADER = y.tab.h
EXECUTABLE = parser

# Default target
all: $(EXECUTABLE)

# Rule to compile the final executable
$(EXECUTABLE): $(LEX_OUTPUT) $(YACC_OUTPUT)
	$(CC) -o $@ $(LEX_OUTPUT) $(YACC_OUTPUT) $(CFLAGS)

# Rule to generate the lex.yy.c file
$(LEX_OUTPUT): $(LEX_FILE) $(YACC_HEADER)
	$(FLEX) $(LEX_FILE)

# Rule to generate the y.tab.c and y.tab.h files
$(YACC_OUTPUT) $(YACC_HEADER): $(YACC_FILE)
	$(BISON) -d $(YACC_FILE)

# Clean rule to remove generated files
clean:
	rm -f $(LEX_OUTPUT) $(YACC_OUTPUT) $(YACC_HEADER) $(EXECUTABLE)

.PHONY: all clean
