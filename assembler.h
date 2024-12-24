#ifndef _ASSEMBLER_H
#define _ASSEMBLER_H
#include <ctype.h>
#include <stdlib.h>
#include "emulator.h"

#define LINELEN 100

#define SKIPSPACES(t) while (isspace(*((t)++))); (t)--

// Stores information about a future reference or a literal constant
// to be filled in later.
typedef struct {
  int addr;
  bool resolved;
  bool which;              // False for sym, true for literal.
  char sym[11];
  word literal;
} futureref;

#define MAXSYMS 1000
typedef struct {
  int star;
  int numsyms, numfuturerefs;
  char syms[MAXSYMS][11];
  word symvals[MAXSYMS];
  futureref futurerefs[MAXSYMS];
  int localsymcounts[10];  // The current number of instances of each local sym nH.
} parsestate;

// Returned by reference in parseline() for mmm to use.
typedef struct {
  bool setdebugline;
  bool isend;
} extraparseinfo;

void initparsestate(parsestate *ps);

bool lookupsym(char *sym, word *val, parsestate *ps);

bool parsesym(char **s, char *sym);
bool parseOP(char **s, char *op, int *opidx);
bool parsenum(char **s, int *val);

// parseatomic() and the rest all write into a word instead of an int,
// because words and ints are NOT equivalent.
// Specifically, +0 != -0.

bool parseatomic(char **s, word *val, parsestate *ps);
bool parseexpr(char **s, word *val, parsestate *ps);
bool parseA(char **s, word *val, parsestate *ps);
bool parseI(char **s, word *val, parsestate *ps);
bool parseF(char **s, word *val, parsestate *ps);
bool parseW(char **s, word *val, parsestate *ps);

bool parseline(char *line, parsestate *ps, mix *mix, extraparseinfo *extraparseinfo);
#endif
