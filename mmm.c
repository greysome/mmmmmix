#include <ctype.h>
#include <stdlib.h>
#include "emulator.h"
#include "assembler.h"

typedef struct {
  mix mix;
  char globalcardfile[LINELEN];
  char debuglines[4000][LINELEN];
} mmmstate;

static mmmstate mmm;

#define RED(s)   "\033[31m" s "\033[37m"
#define GREEN(s) "\033[32m" s "\033[37m"
#define CYAN(s)  "\033[36m" s "\033[37m"

void displayshort(word w) {
  bool sign = SIGN(w);
  byte b4 = (w >>  6) & ONES(6);
  byte b5 =  w        & ONES(6);
  printf("%c %02d %02d", sign ? '+' : '-', b4, b5);
}

void displayword(word w) {
  bool sign = SIGN(w);
  byte b1 = (w >> 24) & ONES(6);
  byte b2 = (w >> 18) & ONES(6);
  byte b3 = (w >> 12) & ONES(6);
  byte b4 = (w >>  6) & ONES(6);
  byte b5 =  w        & ONES(6);
  printf("%c %02d %02d %02d %02d %02d", sign ? '+' : '-', b1, b2, b3, b4, b5);
}

void displayinstr(word w) {
  bool sign = (getA(w) >> 12) & 1;
  word A_abs = getA(w) & ONES(12);
  byte I = getI(w);
  byte F = getF(w);
  byte C = getC(w);
  printf("%c %04d %02d %02d %02d", sign ? '+' : '-', A_abs, I, F, C);
}

int displayfields(bool sign, word A_abs, byte I, byte F, byte default_F) {
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

int pprintinstr(word w) {
  bool sign = (getA(w) >> 12) & 1;
  word A_abs = getA(w) & ONES(12);
  byte I = getI(w);
  byte F = getF(w);
  byte C = getC(w);

#define PP(s,DEFAULTF) { printf(s "\t"); return 8 + displayfields(sign, A_abs, I, F, DEFAULTF); }
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

int showinstrataddr(int i) {
  if (strnlen(mmm.debuglines[i], LINELEN) == 0) {
    putchar('\t');
    return 8 + pprintinstr(mmm.mix.mem[i]);
  }
  else
    return printf(mmm.debuglines[i]);
}

void printregisters() {
  printf(" A: "); displayword(mmm.mix.A);
  printf("\t("); printf("%d", INT(mmm.mix.A));
  printf(")\n X: "); displayword(mmm.mix.X);
  printf("\t("); printf("%d", INT(mmm.mix.X));
  printf(")\nI1: "); displayshort(mmm.mix.Is[0]);
  printf("\t("); printf("%d", INT(mmm.mix.Is[0]));
  printf(")\nI2: "); displayshort(mmm.mix.Is[1]);
  printf("\t("); printf("%d", INT(mmm.mix.Is[1]));
  printf(")\nI3: "); displayshort(mmm.mix.Is[2]);
  printf("\t("); printf("%d", INT(mmm.mix.Is[2]));
  printf(")\nI4: "); displayshort(mmm.mix.Is[3]);
  printf("\t("); printf("%d", INT(mmm.mix.Is[3]));
  printf(")\nI5: "); displayshort(mmm.mix.Is[4]);
  printf("\t("); printf("%d", INT(mmm.mix.Is[4]));
  printf(")\nI6: "); displayshort(mmm.mix.Is[5]);
  printf("\t("); printf("%d", INT(mmm.mix.Is[5]));
  printf(")\n J: "); displayshort(mmm.mix.J);
  printf("\t("); printf("%d", INT(mmm.mix.J));
  printf(")\n\nOverflow: ");
  if (mmm.mix.overflow)
    printf("ON\n");
  else
    printf("OFF\n");
  printf("Comparison: ");
  if (mmm.mix.cmp == -1) putchar('<');
  else if (mmm.mix.cmp == 1) putchar('>');
  else putchar('=');
  printf("\nCur instruction:\n%04d\t", mmm.mix.PC);
  printf("(");
  displayinstr(mmm.mix.mem[mmm.mix.PC]);
  printf(")\t");
  showinstrataddr(mmm.mix.PC);
  putchar('\n');
}

int numdigits(int x) {
  if (x == 0) return 1;
  int i = 0;
  while ((x = x/10) > 0)
    i++;
  return i;
}

void printtime() {
  int totaltime = 0;
  int maxdigits = 0;
  for (int i = 0; i < 4000; i++) {
    totaltime += mmm.mix.exectimes[i];
    int nd = numdigits(mmm.mix.execcounts[i]);
    if (nd > maxdigits)
      maxdigits = nd;
  }
  printf("Time taken: %du\n\n", totaltime);
  printf("BREAKDOWN\n");
  for (int i = 0; i < 4000; i++) {
    int count = mmm.mix.execcounts[i];
    int numchars;
    if (count == 1)
      numchars = printf("%04d: %*c%d  time, %du", i, maxdigits-numdigits(count)+1, ' ', count, mmm.mix.exectimes[i]);
    else if (count > 1)
      numchars = printf("%04d: %*c%d times, %du", i, maxdigits-numdigits(count)+1, ' ', count, mmm.mix.exectimes[i]);
    
    if (count >= 1) {
      if (numchars < 24)
	printf("\t\t");
      else if (numchars < 32)
	printf("\t");
      showinstrataddr(i);
      putchar('\n');
    }
  }
}

bool onestepwrapper(mix *mix) {
  onestep(mix);
  if (*mix->err != '\0') {
    printf(RED("Emulator stopped at %d: %s") "\n", mix->PC, mix->err);
    return false;
  }
  return true;
}

void viewcommand(char *line) {
  SKIPSPACES(line);
  char *fromstart = line, *tostart;
  int from, to;
  while (!isspace(*line) && *line != '-')
    line++;
  if (*line == '-') {
    *(line++) = '\0';
    tostart = line;
    while (!isspace(*(line++))) {};
    *(line-1) = '\0';
    from = atoi(fromstart);
    to = atoi(tostart);
  }
  else {
    *line = '\0';
    from = atoi(fromstart);
    to = from;
  }
  for (int i = from; i <= to; i++)  {
    if (i < 0 || i >= 4000) {
      printf(RED("Invalid memory address %04d") "\n", from);
      return;
    }
    printf("%04d: ", i);
    displayinstr(mmm.mix.mem[i]);
    putchar('\t');
    displayword(mmm.mix.mem[i]);
    printf("   ");
    if (INT(mmm.mix.mem[i]) >= 100 || (int)INT(mmm.mix.mem[i]) <= -10)
      printf("(%d)\t", INT(mmm.mix.mem[i]));
    else
      printf("(%d)\t\t", INT(mmm.mix.mem[i]));
    showinstrataddr(i);
    putchar('\n');
  }
}

void loadcardfile(char *filename);
void cardfilecommand(char *line) {
  SKIPSPACES(line);
  char *start = line;
  while (!isspace(*(line++))) {};
  *(line-1) = '\0';
  loadcardfile(start);
}

void breakpointcommand(char *line, mix *mix) {
  SKIPSPACES(line);
  char *start = line;
  while (!isspace(*line)) line++;
  *line = '\0';
  int bp = atoi(start);
  if (bp < 0 || bp >= 4000) {
    printf(RED("Breakpoint must be between 0-4000") "\n");
    return;
  }
  do {
    if (!onestepwrapper(mix))
      break;
  } while (mix->PC != bp);
}

void loadcardfile(char *filename) {
  FILE *fp;
  if ((fp = fopen(filename, "r")) == NULL) {
    printf(RED("Could not open card file %s") "\n", filename);
    return;
  }
  printf(GREEN("Loaded card file %s") "\n", filename);
  if (mmm.mix.cardfile)
    fclose(mmm.mix.cardfile);
  mmm.mix.cardfile = fp;
  strncpy(mmm.globalcardfile, filename, LINELEN);
}

void loadmixalfile(char *filename) {
  initmix(&mmm.mix);
  parsestate ps;
  initparsestate(&ps);

  FILE *fp;
  if ((fp = fopen(filename, "r")) == NULL) {
    printf(RED("Could not open MIXAL file %s") "\n", filename);
    return;
  }
  int linenum = 0;
  char line[LINELEN];
  bool debuggable;
  while (++linenum && fgets(line, LINELEN, fp) != NULL) {
    if (!parseline(line, &ps, &mmm.mix, &debuggable)) {
      printf(RED("Error at line %d") "\n", linenum);
      initmix(&mmm.mix);
      return;
    }
    if (debuggable) {
      strncpy(mmm.debuglines[ps.star-1], line, LINELEN);
      int len = strlen(line);
      mmm.debuglines[ps.star-1][len-1] = '\0';
    }
  }
  printf(GREEN("Loaded MIXAL file %s") "\n", filename);
}

void printhelp() {
  printf("Commands:\n");
  printf("c <file>\t\tload card file\n");
  printf("s\t\t\trun one step\n");
  printf("b <line>\t\trun till breakpoint\n");
  printf("g\t\t\trun whole program\n");
  printf("v <start>-<end>\t\tview memory cells\n");
  printf("r\t\t\tview registers and flags\n");
  printf("t\t\t\tprint timing statistics\n");
  printf("h\t\t\tprint this help\n");
}

int main(int argc, char **argv) {
  mmm.globalcardfile[0] = '\0';
  for (int i = 0; i < 4000; i++)
    mmm.debuglines[i][0] = '\0';

  if (argc < 2) {
    printf(RED("Please specify a filename!") "\n");
    return 0;
  }
  loadmixalfile(argv[1]);
  if (argc >= 3)
    loadcardfile(argv[2]);
  printf(CYAN("MIX Management Module, by wyan") "\n");
  printf("Type h for help\n");

  char line[LINELEN+1];
  while (true) {
    printf(">> ");
    fgets(line, LINELEN, stdin);
    if (feof(stdin))
      return 0;

    if (line[0] == 'l' && isspace(line[1])) {       // Reload MIXAL file
      loadmixalfile(argv[1]);
      loadcardfile(mmm.globalcardfile);
    }
    else if (line[0] == 'c' && isspace(line[1]))    // Load card file
      cardfilecommand(line+1);

    else if (line[0] == 's' && isspace(line[1])) {  // Run one step
      if (mmm.mix.done) {
	printf(GREEN("Program has finished running") "\n");
	continue;
      }
      onestepwrapper(&mmm.mix);
      printf("%04d\t", mmm.mix.PC);
      showinstrataddr(mmm.mix.PC);
      putchar('\n');
    }

    else if (line[0] == 'b' && isspace(line[1]))    // Run till breakpoint
      breakpointcommand(line+1, &mmm.mix);

    else if (line[0] == 'g' && isspace(line[1])) {  // Run whole program
      while (!mmm.mix.done)
	onestepwrapper(&mmm.mix);
    }

    else if (line[0] == 'v' && isspace(line[1]))    // View memory
      viewcommand(line+1);
    else if (line[0] == 'r' && isspace(line[1]))    // View registers + flags
      printregisters();
    else if (line[0] == 't' && isspace(line[1]))    // View timing statistics
      printtime();
    else if (line[0] == 'h' && isspace(line[1]))    // Help
      printhelp();
  }
}