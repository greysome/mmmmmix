CFLAGS = -g

all: mmm
mmm: mmm.c emulator.c assembler.c
test: test.c emulator.c assembler.c