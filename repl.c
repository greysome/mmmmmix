#include <ctype.h>
#include <stdlib.h>
#include "emulator.h"
#include "assembler.h"

#define LINELEN 32

mix amix;

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

void printregisters() {
  printf(" A: "); displayword(amix.A);
  printf("\t X: "); displayword(amix.X);
  printf("\nI1: "); displayshort(amix.Is[0]);
  printf("\t\tI2: "); displayshort(amix.Is[1]);
  printf("\nI3: "); displayshort(amix.Is[2]);
  printf("\t\tI4: "); displayshort(amix.Is[3]);
  printf("\nI5: "); displayshort(amix.Is[4]);
  printf("\t\tI6: "); displayshort(amix.Is[5]);
  printf("\n J: "); displayshort(amix.J);
  printf("\n\nOverflow: ");
  if (amix.overflow)
    printf("ON\n");
  else
    printf("OFF\n");
  printf("Comparison: ");
  if (amix.cmp == -1) putchar('<');
  else if (amix.cmp == 1) putchar('>');
  else putchar('=');
  printf("\nPC: %04d\n", amix.PC);
  printf("Cur instruction: ");
  displayinstr(amix.mem[amix.PC]);
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
  for (int i = 0; i <= 4000; i++) {
    totaltime += amix.exectimes[i];
    int nd = numdigits(amix.execcounts[i]);
    if (nd > maxdigits)
      maxdigits = nd;
  }
  printf("Time taken: %du\n\n", totaltime);
  printf("BREAKDOWN\n");
  for (int i = 0; i <= 4000; i++) {
    int count = amix.execcounts[i];
    if (count == 1)
      printf("%04d: %*c%d  time, %du\n", i, maxdigits-numdigits(count)+1, ' ', count, amix.exectimes[i]);
    else if (count > 1)
      printf("%04d: %*c%d times, %du\n", i, maxdigits-numdigits(count)+1, ' ', count, amix.exectimes[i]);
  }
}

void parseload(char *line) { 
  SKIPSPACES(line);
  char *start = line;
  while (!isspace(*(line++))) {};
  *(line-1) = '\0';

  initmix(&amix);
  parsestate ps;
  initparsestate(&ps);

  FILE *fp;
  if ((fp = fopen(start, "r")) == NULL) {
    printf("Could not open %s\n", start);
    return;
  }
  int linenumber = 0;
  char fileline[LINELEN];
  while (++linenumber && fgets(fileline, LINELEN, fp) != NULL) {
    if (!parseline(fileline, &ps, &amix)) {
      printf("Error at line %d\n", linenumber);
      initmix(&amix);
      return;
    }
  }
  printf("Loaded %s\n", start);
}

void parseview(char *line) {
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
      printf("Invalid memory address %04d\n", from);
      return;
    }
    printf("%04d: ", i);
    displayinstr(amix.mem[i]);
    putchar('\n');
  }
}

int main() {
  initmix(&amix);
  amix.PC = 3000;
  char line[LINELEN+1];
  while (true) {
    printf(">> ");
    fgets(line, LINELEN, stdin);
    if (feof(stdin))
      return 0;
    if (line[0] == 'l' && isspace(line[1]))
      parseload(line+1);
    else if (line[0] == 'v' && isspace(line[1]))
      parseview(line+1);
    else if (line[0] == 't' && isspace(line[1]))
      printtime();
    else if (line[0] == 'r' && isspace(line[1]))
      printregisters();
    else if (line[0] == 's' && isspace(line[1])) {
      if (!amix.done)
	onestep(&amix);
    }
    else if (line[0] == 'g' && isspace(line[1])) {
      while (!amix.done)
	onestep(&amix);
    }
  }
}