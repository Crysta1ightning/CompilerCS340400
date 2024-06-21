# Makefile for compiling and running the Flex scanner

# Define the compiler
CC = gcc

# Flex compiler
FLEX = flex

# The name of the executable
TARGET = scanner

# The source files
LEX_SOURCE = scanner.l
LEX_OUTPUT = lex.yy.c

all: $(TARGET)

$(TARGET): $(LEX_OUTPUT)
	$(CC) $(CFLAGS) $(LEX_OUTPUT) -lfl -o $(TARGET)

$(LEX_OUTPUT): $(LEX_SOURCE)
	$(FLEX) $(LEX_SOURCE)

clean:
	rm -f $(LEX_OUTPUT) $(TARGET)

run: $(TARGET)
	./$(TARGET)