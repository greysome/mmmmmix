CFLAGS = -g

all: repl
repl: repl.c emulator.c assembler.c
test: test.c emulator.c assembler.c