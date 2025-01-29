// MIX MANAGEMENT MODULE

#include <ctype.h>
#include <stdlib.h>
#include "emulator.h"
#include "assembler.h"

typedef struct {
  mix mix;
  parsestate ps;
  char prevline[LINELEN];
  char globalcardfile[LINELEN];
  char globaltapefiles[8][LINELEN];
  char debuglines[4000][LINELEN];
  bool shouldtrace;
} mmmstate;

#define RED(s)    "\033[31m" s "\033[37m"
#define GREEN(s)  "\033[32m" s "\033[37m"
#define YELLOW(s) "\033[33m" s "\033[37m"
#define BLUE(s)   "\033[34m" s "\033[37m"
#define CYAN(s)   "\033[36m" s "\033[37m"

// Similar to printf, the following display functions return the
// *visual width* of the printed string.
// Thus a string like "\t" has width 8.

int displayshort(word w) {
  bool sign = SIGN(w);
  byte b4 = (w >> 6) & ONES(6);
  byte b5 =  w       & ONES(6);
  printf("%c %02d %02d", sign ? '+' : '-', b4, b5);
  return 1+2*3;
}

int displayword(word w) {
  bool sign = SIGN(w);
  byte b1 = (w >> 24) & ONES(6);
  byte b2 = (w >> 18) & ONES(6);
  byte b3 = (w >> 12) & ONES(6);
  byte b4 = (w >>  6) & ONES(6);
  byte b5 =  w        & ONES(6);
  printf("%c %02d %02d %02d %02d %02d", sign ? '+' : '-', b1, b2, b3, b4, b5);
  return 1+5*3;
}

// Display instruction in a format similar to displayword, but with
// first two bytes combined
int displayinstr_raw(word w) {
  bool sign = (getA(w) >> 12) & 1;
  word A_abs = getA(w) & ONES(12);
  byte I = getI(w);
  byte F = getF(w);
  byte C = getC(w);
  printf("%c %04d %02d %02d %02d", sign ? '+' : '-', A_abs, I, F, C);
  return 1+5+3*3;
}

// Display the A,I(F) portion of the canonical representation of an
// instruction
int _displayinstr_fields(bool sign, word A_abs, byte I, byte F, byte default_F) {
  int numchars = 0;
  if (!sign) {
    numchars++;
    putchar('-');
  }
  numchars += printf("%d", A_abs);
  if (I != 0)
    numchars += printf(",%d", I);
  if (F != default_F)
    numchars += printf("(%d)", F);
  return numchars;
}

// Display the canonical representation of an instruction, or ??? if
// invalid instruction.
int displayinstr_canonical(word w) {
  bool sign = (getA(w) >> 12) & 1;
  word A_abs = getA(w) & ONES(12);
  byte I = getI(w);
  byte F = getF(w);
  byte C = getC(w);

#define PP(s,DEFAULTF) { printf(s "\t"); return 8 + _displayinstr_fields(sign, A_abs, I, F, DEFAULTF); }
#define UNKNOWN()      { printf("???"); return 3; }

  if (C == 0) PP("NOP", 0)
  else if (C == 1) PP("ADD", 5)
  else if (C == 2) PP("SUB", 5)
  else if (C == 3) PP("MUL", 5)
  else if (C == 4) PP("DIV", 5)
  else if (C == 5) {
    if (F == 0) PP("MUL", 0)
    else if (F == 1) PP("CHAR", 1)
    else if (F == 2) PP("HLT", 2)
    else UNKNOWN()
  }
  else if (C == 6) {
    if (F == 0) PP("SLA", 0)
    else if (F == 1) PP("SRA", 1)
    else if (F == 2) PP("SLAX", 2)
    else if (F == 3) PP("SRAX", 3)
    else if (F == 4) PP("SLC", 4)
    else if (F == 5) PP("SRC", 5)
    else UNKNOWN()
  }
  else if (C == 7) PP("MOVE", 1)
  else if (C == 8) PP("LDA", 5)
  else if (C == 9) PP("LD1", 5)
  else if (C == 10) PP("LD2", 5)
  else if (C == 11) PP("LD3", 5)
  else if (C == 12) PP("LD4", 5)
  else if (C == 13) PP("LD5", 5)
  else if (C == 14) PP("LD6", 5)
  else if (C == 15) PP("LDX", 5)
  else if (C == 16) PP("LDAN", 5)
  else if (C == 17) PP("LD1N", 5)
  else if (C == 18) PP("LD2N", 5)
  else if (C == 19) PP("LD3N", 5)
  else if (C == 20) PP("LD4N", 5)
  else if (C == 21) PP("LD5N", 5)
  else if (C == 22) PP("LD6N", 5)
  else if (C == 23) PP("LDXN", 5)
  else if (C == 24) PP("STA", 5)
  else if (C == 25) PP("ST1", 5)
  else if (C == 26) PP("ST2", 5)
  else if (C == 27) PP("ST3", 5)
  else if (C == 28) PP("ST4", 5)
  else if (C == 29) PP("ST5", 5)
  else if (C == 30) PP("ST6", 5)
  else if (C == 31) PP("STX", 5)
  else if (C == 32) PP("STJ", 2)
  else if (C == 33) PP("STZ", 5)
  else if (C == 34) PP("JBUS", 0)
  else if (C == 35) PP("IOC", 0)
  else if (C == 36) PP("IN", 0)
  else if (C == 37) PP("OUT", 0)
  else if (C == 38) PP("JRED", 0)
  else if (C == 39) {
    if (F == 0) PP("JMP", 0)
    else if (F == 1) PP("JSJ", 1)
    else if (F == 2) PP("JOV", 2)
    else if (F == 3) PP("JNOV", 3)
    else if (F == 4) PP("JL", 4)
    else if (F == 5) PP("JE", 5)
    else if (F == 6) PP("JG", 6)
    else if (F == 7) PP("JGE", 7)
    else if (F == 8) PP("JNE", 8)
    else if (F == 9) PP("JLE", 9)
    else UNKNOWN()
  }
  else if (C == 40) {
    if (F == 0) PP("JAN", 0)
    else if (F == 1) PP("JAZ", 1)
    else if (F == 2) PP("JAP", 2)
    else if (F == 3) PP("JANN", 3)
    else if (F == 4) PP("JANZ", 4)
    else if (F == 5) PP("JANP", 5)
    else UNKNOWN()
  }
  else if (C == 41) {
    if (F == 0) PP("J1N", 0)
    else if (F == 1) PP("J1Z", 1)
    else if (F == 2) PP("J1P", 2)
    else if (F == 3) PP("J1NN", 3)
    else if (F == 4) PP("J1NZ", 4)
    else if (F == 5) PP("J1NP", 5)
    else UNKNOWN()
  }
  else if (C == 42) {
    if (F == 0) PP("J2N", 0)
    else if (F == 1) PP("J1Z", 1)
    else if (F == 2) PP("J2P", 2)
    else if (F == 3) PP("J2NN", 3)
    else if (F == 4) PP("J2NZ", 4)
    else if (F == 5) PP("J2NP", 5)
    else UNKNOWN()
  }
  else if (C == 43) {
    if (F == 0) PP("J3N", 0)
    else if (F == 1) PP("J3Z", 1)
    else if (F == 2) PP("J3P", 2)
    else if (F == 3) PP("J3NN", 3)
    else if (F == 4) PP("J3NZ", 4)
    else if (F == 5) PP("J3NP", 5)
    else UNKNOWN()
  }
  else if (C == 44) {
    if (F == 0) PP("J4N", 0)
    else if (F == 1) PP("J4Z", 1)
    else if (F == 2) PP("J4P", 2)
    else if (F == 3) PP("J4NN", 3)
    else if (F == 4) PP("J4NZ", 4)
    else if (F == 5) PP("J4NP", 5)
    else UNKNOWN()
  }
  else if (C == 45) {
    if (F == 0) PP("J5N", 0)
    else if (F == 1) PP("J5Z", 1)
    else if (F == 2) PP("J5P", 2)
    else if (F == 3) PP("J5NN", 3)
    else if (F == 4) PP("J5NZ", 4)
    else if (F == 5) PP("J5NP", 5)
    else UNKNOWN()
  }
  else if (C == 46) {
    if (F == 0) PP("J6N", 0)
    else if (F == 1) PP("J6Z", 1)
    else if (F == 2) PP("J6P", 2)
    else if (F == 3) PP("J6NN", 3)
    else if (F == 4) PP("J6NZ", 4)
    else if (F == 5) PP("J6NP", 5)
    else UNKNOWN()
  }
  else if (C == 47) {
    if (F == 0) PP("JXN", 0)
    else if (F == 1) PP("JXZ", 1)
    else if (F == 2) PP("JXP", 2)
    else if (F == 3) PP("JXNN", 3)
    else if (F == 4) PP("JXNZ", 4)
    else if (F == 5) PP("JXNP", 5)
    else UNKNOWN()
  }
  else if (C == 48) {
    if (F == 0) PP("INCA", 0)
    else if (F == 1) PP("DECA", 1)
    else if (F == 2) PP("ENTA", 2)
    else if (F == 3) PP("ENNA", 3)
    else UNKNOWN()
  }
  else if (C == 49) {
    if (F == 0) PP("INC1", 0)
    else if (F == 1) PP("DEC1", 1)
    else if (F == 2) PP("ENT1", 2)
    else if (F == 3) PP("ENN1", 3)
    else UNKNOWN()
  }
  else if (C == 50) {
    if (F == 0) PP("INC2", 0)
    else if (F == 1) PP("DEC2", 1)
    else if (F == 2) PP("ENT2", 2)
    else if (F == 3) PP("ENN2", 3)
    else UNKNOWN()
  }
  else if (C == 51) {
    if (F == 0) PP("INC3", 0)
    else if (F == 1) PP("DEC3", 1)
    else if (F == 2) PP("ENT3", 2)
    else if (F == 3) PP("ENN3", 3)
    else UNKNOWN()
  }
  else if (C == 52) {
    if (F == 0) PP("INC4", 0)
    else if (F == 1) PP("DEC4", 1)
    else if (F == 2) PP("ENT4", 2)
    else if (F == 3) PP("ENN4", 3)
    else UNKNOWN()
  }
  else if (C == 53) {
    if (F == 0) PP("INC5", 0)
    else if (F == 1) PP("DEC5", 1)
    else if (F == 2) PP("ENT5", 2)
    else if (F == 3) PP("ENN5", 3)
    else UNKNOWN()
  }
  else if (C == 54) {
    if (F == 0) PP("INC6", 0)
    else if (F == 1) PP("DEC6", 1)
    else if (F == 2) PP("ENT6", 2)
    else if (F == 3) PP("ENN6", 3)
    else UNKNOWN()
  }
  else if (C == 55) {
    if (F == 0) PP("INCX", 0)
    else if (F == 1) PP("DECX", 1)
    else if (F == 2) PP("ENTX", 2)
    else if (F == 3) PP("ENNX", 3)
    else UNKNOWN()
  }
  else if (C == 56) PP("CMPA", 5)
  else if (C == 57) PP("CMP1", 5)
  else if (C == 58) PP("CMP2", 5)
  else if (C == 59) PP("CMP3", 5)
  else if (C == 60) PP("CMP4", 5)
  else if (C == 61) PP("CMP5", 5)
  else if (C == 62) PP("CMP6", 5)
  else if (C == 63) PP("CMPX", 5)
  else UNKNOWN()
}

// Display the MIXAL source resulting in that instruction.
// This differs from the canonical representation in that there may be
// user-defined constants.
// If MIXAL source is not available, fall back to the canonical representation.
void displayinstr_mixal(int i, mmmstate *mmm) {
  if (i < 0 || i >= 4000) {
    printf(RED("Invalid memory address %04d\n"), i);
    return;
  }
  if (mmm->debuglines[i][0] == '\0') {
    putchar('\t');
    displayinstr_canonical(mmm->mix.mem[i]);
  }
  else
    printf(mmm->debuglines[i]);
}

// Display the instruction as a complete line, for debugging purposes.
// The following format is used:
// LINENUM:EXECCOUNT +- AAAA I F C            MIXAL or canonical
void displayinstr_debug(int i, mmmstate *mmm) {
  int execcount = mmm->mix.execcounts[i];
  printf(BLUE("%04d:%d "), i, execcount+1);
  printf("\033[33m");
  displayinstr_raw(mmm->mix.mem[i]);
  printf("\033[37m\t");
  displayinstr_mixal(i, mmm);
  putchar('\n');
}

// Display the memory address as a complete line, with lots of information.
// The following format is used:
// LINENUM +- AAAA I F C  +- B1 B2 B3 B4 B5  (integer value)           MIXAL or canonical
void displayaddr_verbose(int i, mmmstate *mmm) {
  if (i < 0 || i >= 4000) {
    printf(RED("Invalid memory address %04d\n"), i);
    return;
  }
  // Display currently running instruction in green
  if (i == mmm->mix.PC) {
    printf("\033[32m%04d ", i);
    displayinstr_raw(mmm->mix.mem[i]);
    printf("  ");
    displayword(mmm->mix.mem[i]);
    printf("  ");
    // If the number of chars to display the integer value of memory
    // contents exceeds a certain amount
    if (INT(mmm->mix.mem[i]) >= 100000 || (int)INT(mmm->mix.mem[i]) <= -10000)
      printf("(%d)\t", INT(mmm->mix.mem[i]));
    else
      printf("(%d)\t\t", INT(mmm->mix.mem[i]));
    displayinstr_mixal(i, mmm);
    printf("\033[37m\n");
  }
  // Display line in normal formatting
  else {
    printf(BLUE("%04d "), i);
    printf("\033[33m");
    displayinstr_raw(mmm->mix.mem[i]);
    printf("  ");
    displayword(mmm->mix.mem[i]);
    printf("\033[37m  ");
    if (INT(mmm->mix.mem[i]) >= 100000 || (int)INT(mmm->mix.mem[i]) <= -10000)
      printf(CYAN("(%d)\t"), INT(mmm->mix.mem[i]));
    else
      printf(CYAN("(%d)\t\t"), INT(mmm->mix.mem[i]));
    displayinstr_mixal(i, mmm);
    putchar('\n');
  }
}

void printregisters(mmmstate *mmm) {
  printf(" A: ");
  printf("\033[33m"); displayword(mmm->mix.A); printf("\033[37m");
  printf("\t");
  printf(CYAN("(%d)"), INT(mmm->mix.A));

  printf("\n X: ");
  printf("\033[33m"); displayword(mmm->mix.X); printf("\033[37m");
  printf("\t");
  printf(CYAN("(%d)"), INT(mmm->mix.X));

  printf("\nI1: ");
  printf("\033[33m"); displayshort(mmm->mix.Is[0]); printf("\033[37m");
  printf("\t"); printf(CYAN("(%d)"), INT(mmm->mix.Is[0]));

  printf("\nI2: ");
  printf("\033[33m"); displayshort(mmm->mix.Is[1]); printf("\033[37m");
  printf("\t"); printf(CYAN("(%d)"), INT(mmm->mix.Is[1]));

  printf("\nI3: ");
  printf("\033[33m"); displayshort(mmm->mix.Is[2]); printf("\033[37m");
  printf("\t"); printf(CYAN("(%d)"), INT(mmm->mix.Is[2]));

  printf("\nI4: ");
  printf("\033[33m"); displayshort(mmm->mix.Is[3]); printf("\033[37m");
  printf("\t"); printf(CYAN("(%d)"), INT(mmm->mix.Is[3]));

  printf("\nI5: ");
  printf("\033[33m"); displayshort(mmm->mix.Is[4]); printf("\033[37m");
  printf("\t"); printf(CYAN("(%d)"), INT(mmm->mix.Is[4]));

  printf("\nI6: ");
  printf("\033[33m"); displayshort(mmm->mix.Is[5]); printf("\033[37m");
  printf("\t"); printf(CYAN("(%d)"), INT(mmm->mix.Is[5]));

  printf("\n J: ");
  printf("\033[33m"); displayshort(mmm->mix.J); printf("\033[37m");
  printf("\t"); printf(CYAN("(%d)"), INT(mmm->mix.J));

  printf("\n\nOverflow: ");
  if (mmm->mix.overflow)
    printf(CYAN("ON\n"));
  else
    printf(CYAN("OFF\n"));
  printf("Comparison: ");
  if (mmm->mix.cmp == -1)
    printf(CYAN("<"));
  else if (mmm->mix.cmp == 1)
    printf(CYAN(">"));
  else
    printf(CYAN("="));

  printf("\n\nBusy IO devices:\n");
  for (int i = 0; i < 21; i++) {
    IOthread iothread = mmm->mix.iothreads[i];
    if (iothread.timer == 0)
      continue;
    if (iothread.C == 35)
      printf(GREEN("%d") CYAN("  IOC") "  (%du left)\n", i, iothread.timer);
    else if (iothread.C == 36)
      printf(GREEN("%d") CYAN("  IN ") "  (%du left), address = %04d\n", i, iothread.timer, INT(iothread.M));
    else if (iothread.C == 37)
      printf(GREEN("%d") CYAN("  OUT") "  (%du left), address = %04d\n", i, iothread.timer, INT(iothread.M));
  }

  printf("\nCur instruction:\n");
  displayinstr_debug(mmm->mix.PC, mmm);
}

int numdigits(int x) {
  if (x == 0) return 1;
  int i = 0;
  while ((x = x/10) > 0)
    i++;
  return i;
}

void printtime(mmmstate *mmm) {
  int totaltime = 0;
  int maxdigits = 0;
  for (int i = 0; i < 4000; i++) {
    totaltime += mmm->mix.exectimes[i];
    int nd = numdigits(mmm->mix.execcounts[i]);
    if (nd > maxdigits)
      maxdigits = nd;
  }
  printf("Time taken: %du\n\n", totaltime);
  printf(CYAN("BREAKDOWN\n"));
  for (int i = 0; i < 4000; i++) {
    int count = mmm->mix.execcounts[i];
    int numchars;
    if (count == 1)
      numchars = printf(BLUE("%04d ") YELLOW("%*c%d  time, %du"), i,
			maxdigits-numdigits(count)+1, ' ', count, mmm->mix.exectimes[i]);
    else if (count > 1)
      numchars = printf(BLUE("%04d ") YELLOW("%*c%d times, %du"), i,
			maxdigits-numdigits(count)+1, ' ', count, mmm->mix.exectimes[i]);

    // Compensate for the \033 sequences using up 20 characters, since
    // they don't move the cursor right
    numchars -= 20;

    if (count >= 1) {
      if (numchars < 24)
	printf("\t\t");
      else if (numchars < 32)
	printf("\t");
      displayinstr_mixal(i, mmm);
      putchar('\n');
    }
  }
}

bool onestepwrapper(int tracecount, mmmstate *mmm) {
  int execcount = mmm->mix.execcounts[mmm->mix.PC];
  if (execcount < tracecount) {
    if (!mmm->shouldtrace) {
      printf("----------------------------------------------------------------------------------------------\n");
      mmm->shouldtrace = true;
    }
    displayinstr_debug(mmm->mix.PC, mmm);
  }
  else {
    mmm->shouldtrace = false;
  }
  onestep(&mmm->mix);
  if (mmm->mix.err[0] != '\0') {
    printf(RED("Emulator stopped at %d: %s\n"), mmm->mix.PC, mmm->mix.err);
    return false;
  }
  return true;
}

bool getsymvalue(char *s, mmmstate *mmm, word *w) {
  // Find the symbol in parse state
  for (int i = 0; i < MAXSYMS; i++) {
    if (strncmp(s, mmm->ps.syms[i], 11) == 0) {
      *w = mmm->ps.symvals[i];
      return true;
    }
  }
  return false;
}

void viewcommand(char *arg, mmmstate *mmm) {
  // Print all nonzero memory addresses
  if (arg[0] == '\0') {
    for (int i = 0; i < 4000; i++) {
      if (mmm->mix.mem[i] != POS(0))
	displayaddr_verbose(i, mmm);
    }
  }
  // Print selected addresses based on arg, where
  // arg = "<from>-<to>" or "<arg>"
  else if (isdigit(arg[0])) {
    char *fromstart = arg, *tostart;
    int from, to;
    while (!isspace(*arg) && *arg != '-')
      arg++;
    if (*arg == '-') {
      *(arg++) = '\0';
      tostart = arg;
      while (!isspace(*(arg++))) {};
      *(arg-1) = '\0';
      from = atoi(fromstart);
      to = atoi(tostart);
    }
    else {
      *arg = '\0';
      from = atoi(fromstart);
      to = from;
    }
    for (int i = from; i <= to; i++)
      displayaddr_verbose(i, mmm);
  }
  // Print address corresponding to the given symbol
  else if (arg[0] == '.') {
    word w;
    if (getsymvalue(arg+1, mmm, &w))
      displayaddr_verbose(INT(w), mmm);
    else
      printf(BLUE("I'm not aware of the symbol %s\n"), arg);
  }
}

void breakpointcommand(char *arg, mmmstate *mmm) {
  int bp;
  if (isdigit(arg[0])) {
    bp = atoi(arg);
  }
  else if (arg[0] == '.') {
    word w;
    if (!getsymvalue(arg+1, mmm, &w)) {
      printf(BLUE("I'm not aware of the symbol %s\n"), arg);
      return;
    }
    bp = INT(w);
  }

  if (bp < 0 || bp >= 4000) {
    printf(RED("Breakpoint must be between 0-4000\n"));
    return;
  }
  do {
    if (!onestepwrapper(0, mmm))
      break;
  } while (mmm->mix.PC != bp);
  displayinstr_debug(bp, mmm);
}

void gocommand(char *arg, mmmstate *mmm) {
  int tracecount;
  if (isdigit(arg[0]))
    tracecount = arg[0] - '0';
  else if (arg[0] == '+')
    tracecount = 2147483647;
  else
    tracecount = 0;
  mmm->shouldtrace = true;
  while (!mmm->mix.done)
    onestepwrapper(tracecount, mmm);
  printf(GREEN("Program has finished running; type l to reset\n"));
}

bool loadcardfile(char *filename, mmmstate *mmm) {
  if (filename[0] == '\0')
    return true;
  FILE *fp;
  if ((fp = fopen(filename, "r")) == NULL) {
    printf(RED("Could not open card file %s\n"), filename);
    return false;
  }
  printf(GREEN("Loaded card file %s\n"), filename);
  if (mmm->mix.cardfile)
    fclose(mmm->mix.cardfile);
  mmm->mix.cardfile = fp;
  return true;
}

bool loadtapefile(char *filename, int n, mmmstate *mmm) {
  if (filename[0] == '\0')
    return true;
  if (n < 0 || n > 7) {
    printf(RED("Invalid tape number %d\n"), n);
    return false;
  }
  FILE *fp;
  if ((fp = fopen(filename, "r+")) == NULL) {
    printf(RED("Could not open tape file %s\n"), filename);
    return false;
  }
  printf(GREEN("Loaded tape file %s\n"), filename);
  if (mmm->mix.tapefiles[n])
    fclose(mmm->mix.tapefiles[n]);
  mmm->mix.tapefiles[n] = fp;
  return true;
}

bool loadmixalfile(char *filename, mmmstate *mmm) {
  initmix(&mmm->mix);
  initparsestate(&mmm->ps);

  FILE *fp;
  if ((fp = fopen(filename, "r")) == NULL) {
    printf(RED("Could not open MIXAL file %s\n"), filename);
    return false;
  }
  int linenum = 0;
  char line[LINELEN];
  extraparseinfo extraparseinfo;
  while (++linenum && fgets(line, LINELEN, fp) != NULL) {
    if (!parseline(line, &mmm->ps, &mmm->mix, &extraparseinfo)) {
      printf(RED("Assembler error at line %d: %s"), linenum, line);
      initmix(&mmm->mix);
      return false;
    }
    if (extraparseinfo.setdebugline) {
      strncpy(mmm->debuglines[mmm->ps.star-1], line, LINELEN);
      int len = strlen(line);
      mmm->debuglines[mmm->ps.star-1][len-1] = '\0';
    }
    if (extraparseinfo.isend)
      break;
  }
  printf(GREEN("Loaded MIXAL file %s\n"), filename);
  return true;
}

static unsigned char mixchr_ascii(byte b) {
  char extra;
  char c = mixchr(b, &extra);
  if (extra == 0x94)
    return '!';
  else if (extra == 0xa3)
    return '[';
  else if (extra == 0xa4)
    return ']';
}

void exportcards(mmmstate *mmm) {
  // The last memory cell that is not +0
  int programend = 0;
  for (int i = 0; i < 4000; i++) {
    if (mmm->mix.mem[i] != POS(0))
      programend = i;
  }
  // "Snap" programend to the end of card
  // e.g. 0-15 -> 15, 16-31 -> 31
  programend += 15 - programend%16;
  // If the program extends to cell 3500 (which would be the end of
  // the 218th card), then it won't fit when loaded as cards into
  // mixsim.mixal. (Cells 3500-3999 are reserved for the simulator
  // logic.)
  if (programend >= 3499) {
    printf(RED("The program goes beyond cell 3500. It cannot be loaded as cards into mixsim.mixal.\n"));
    return;
  }

  FILE *fp;
  if ((fp = fopen("program.cards", "w")) == NULL)
    printf(RED("Could not create file program.cards\n"));

  for (int i = 0; i <= programend; i++) {
    word w = mmm->mix.mem[i];
    if (!SIGN(w)) {
      printf(RED("Negative words cannot be loaded into cards\n"));
      return;
    }
    byte b5 =  w      & ONES(6);
    byte b4 = (w>>6)  & ONES(6);
    byte b3 = (w>>12) & ONES(6);
    byte b2 = (w>>18) & ONES(6);
    byte b1 = (w>>24) & ONES(6);
    printf("%d/%d: %d %d %d %d %d\n", i,programend, b1,b2,b3,b4,b5);
    fputc(mixchr_ascii(b1), fp);
    fputc(mixchr_ascii(b2), fp);
    fputc(mixchr_ascii(b3), fp);
    fputc(mixchr_ascii(b4), fp);
    fputc(mixchr_ascii(b5), fp);
    if (i%16 == 15)  // End of card
      fputc('\n', fp);
  }

  // End-of-program card
  fputs(".....", fp);
  for (int i = 0; i < 75; i++)
    fputc(' ', fp);

  printf(GREEN("Saved program into program.cards.\n"));
}

void printhelp() {
  printf(
    CYAN("COMMANDS:\n")
    "<empty>\t\trepeat previous command\n"
    "l\t\treload MIXAL, card and tape files\n"
    "@<file>\t\tuse card file\n"
    "#<n><file>\tuse tape file\n"
    "s\t\trun one step\n"
    "b<line>\t\trun till specified line\n"
    "b.<sym>\t\trun till specified line\n"
    "g\t\trun whole program\n"
    "g<n>\t\ttrace first n executions of each line (n is a digit)\n"
    "g+\t\ttrace everything\n"
    "v\t\tview nonempty memory cells\n"
    "v<line>\t\tview specific cell\n"
    "v.<sym>\t\tview specific cell\n"
    "v<from>-<to>\tview a range of cells\n"
    "r\t\tview registers and flags\n"
    "t\t\tprint timing statistics\n"
    "C\t\texport program into cards\n"
    "h\t\tprint this help\n"
    "q\t\tquit\n"
  );
}

void initmmmstate(mmmstate *mmm) {
  // mmm->mix will be initialized in loadmixalfile()
  mmm->globalcardfile[0] = '\0';
  for (int i = 0; i < 8; i++)
    mmm->globaltapefiles[i][0] = '\0';
  mmm->prevline[0] = '\0';
  for (int i = 0; i < 4000; i++)
    mmm->debuglines[i][0] = '\0';
  mmm->shouldtrace = true;

  // Default IO operation times
  mmm->mix.INtimes[16] = 10000;
  mmm->mix.IOCtimes[18] = 10000;
  mmm->mix.OUTtimes[18] = 7500;
  // I chose a value arbitrarily for tape IO because I haven't seen an
  // official figure yet
  for (int i = 0; i < 8; i++) {
    mmm->mix.INtimes[i] = 30000;
    mmm->mix.OUTtimes[i] = 30000;
  }
}

int main(int argc, char **argv) {
  mmmstate mmm;
  initmmmstate(&mmm);

  // Handle arguments
  if (argc < 2) {
    printf(RED("Please specify a filename!\n"));
    return 0;
  }
  if (!loadmixalfile(argv[1], &mmm))
    return 0;
  if (argc >= 3 && loadcardfile(argv[2], &mmm))
    strncpy(mmm.globalcardfile, argv[2], LINELEN);

  printf(CYAN("MIX Management Module, by wyan\n"));
  printf("Type h for help\n");

  char line[LINELEN+1];
  while (true) {
    printf(">> ");
    fgets(line, LINELEN, stdin);
    // Strip the last newline
    line[strnlen(line, LINELEN)-1] = '\0';
    if (feof(stdin))
      return 0;

    if (line[0] == '\0') {      // Previous command
      strncpy(line, mmm.prevline, LINELEN);
    }
    strncpy(mmm.prevline, line, LINELEN);

    if (line[0] == 'l') {       // Reload MIXAL, card and tape files
      if (!loadmixalfile(argv[1], &mmm))
	return 0;
      loadcardfile(mmm.globalcardfile, &mmm);
      for (int i = 0; i < 8; i++)
	if (mmm.globaltapefiles[i] != '\0')
	  loadtapefile(mmm.globaltapefiles[i], i, &mmm);
    }
    else if (line[0] == '@') {  // Load new card file
      if (loadcardfile(line+1, &mmm))
	strncpy(mmm.globalcardfile, line+1, LINELEN);
    }
    else if (line[0] == '#') {  // Load new tape file
      if (line[1] == '\0')
	printf(RED("You have to provide a tape number!\n"));
      else {
	int n = line[1]-'0';
	if (loadtapefile(line+2, n, &mmm))
	  strncpy(mmm.globaltapefiles[n], line+2, LINELEN);
      }
    }
    else if (line[0] == 's') {  // Run one step
      if (mmm.mix.done) {
	printf(GREEN("Program has finished running; type l to reset\n"));
	continue;
      }
      onestepwrapper(0, &mmm);
      displayinstr_debug(mmm.mix.PC, &mmm);
    }
    else if (line[0] == 'b')    // Run till breakpoint
      breakpointcommand(line+1, &mmm);
    else if (line[0] == 'g') {  // Run whole program
      gocommand(line+1, &mmm);
    }
    else if (line[0] == 'v')    // View memory
      viewcommand(line+1, &mmm);
    else if (line[0] == 'r')    // View registers + flags
      printregisters(&mmm);
    else if (line[0] == 't')    // View timing statistics
      printtime(&mmm);
    else if (line[0] == 'C')    // Export program into cards
      exportcards(&mmm);
    else if (line[0] == 'h')    // Help
      printhelp();
    else if (line[0] == 'q')    // Quit
      return 0;
    else
      printf(BLUE("Hold up, I don't understand that command\n"));
  }
}