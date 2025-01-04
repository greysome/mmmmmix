#include "emulator.h"

//                                                 v MOVE has variable time
static int instrtimes[64] = { 1, 2, 2,10,12,10, 2,-1,
		              2, 2, 2, 2, 2, 2, 2, 2,
		              2, 2, 2, 2, 2, 2, 2, 2,
		              2, 2, 2, 2, 2, 2, 2, 2,
		              2, 2, 1,-1,-1,-1, 1, 1,  // IN/OUT/IOC have variable time
		              1, 1, 1, 1, 1, 1, 1, 1,
		              1, 1, 1, 1, 1, 1, 1, 1,
		              2, 2, 2, 2, 2, 2, 2, 2};

int max(int a, int b) { return a >= b ? a : b; }

word ADDR(int x) {
  int pos = x < 0 ? -x : x;
  assert(pos <= 1<<12);
  bool sign = !((x >> 15) & 1);
  return pos | (sign << 12);
}

word WORD(bool sign, byte b1, byte b2, byte b3, byte b4, byte b5) {
  assert(b1 < 64); assert(b2 < 64); assert(b3 < 64);
  assert(b4 < 64); assert(b5 < 64);
  word w = b5;
  w |= b4 << 6;
  w |= b3 << 12;
  w |= b2 << 18;
  w |= b1 << 24;
  w |= sign << 30;
  return w;
}

word INSTR(word A, byte I, byte F, byte C) {
  assert(C < 1<<6);
  assert(F <= 45);
  assert(I <= 6);
  assert(A <= 1<<13);
  word w = C;
  w |= F << 6;
  w |= I << 12;
  w |= A << 18;
  return w;
}

word getA(word instr) { return (instr >> 18) & ONES(13); }
byte getI(word instr) { return (instr >> 12) & ONES(6); }
byte getF(word instr) { return (instr >> 6) & ONES(6); }
byte getC(word instr) { return instr & ONES(6); }
word getM(word instr, mix *mix);

bool checkfieldspec(byte F) {
  int start = F/8, end = F%8;
  return 0 <= start && start <= 5 && start <= end && end <= 5;
}

word applyfield(word w, byte F) {
  int start = F/8, end = F%8;
  bool sign = (w >> 30) & 1;
  word v = w >> 6*(5-end);  // Last byte of v = end byte of w
  v &= ONES(6 * (end - max(start,1) + 1));  // Keep the desired bytes based on F
  if (start == 0)
    return WITHSIGN(v, sign);
  else
    return POS(v);
}

void loadword(word *dest, word src) {
  *dest = src;
}

void storeword(word *dest, word src, byte F) {
  int start = F/8, end = F%8;
  word mask = ONES(6 * (end - max(start,1) + 1));
  word touched_bit_positions = mask << 6*(5-end);
  word new_bits = (src & mask) << 6*(5-end);
  if (start == 0) {
    touched_bit_positions |= 1<<30;
    new_bits |= src & (1<<30);
  }
  word untouched_bit_positions = ONES(31) ^ touched_bit_positions;
  *dest &= untouched_bit_positions;  // Clear out the parts of *dest we are about to store
  *dest |= new_bits;
}

word negword(word w) {
  return WITHSIGN(w, !SIGN(w));
}

bool addword(word *dest, word src) {
  bool sign1 = SIGN(*dest);
  bool sign2 = SIGN(src);
  word w1 = MAG(*dest);
  word w2 = MAG(src);
  // Because the signed words are not stored using two's complement
  // notation, the addition has to be split into cases by sign.
  bool overflow;
  if (sign1 && sign2) {
    overflow = (w1+w2) >= 1<<30;
    *dest = POS(w1+w2);
  }
  else if (sign1 && !sign2) {
    overflow = false;
    if (w1 < w2)
      *dest = NEG(w2-w1);
    else
      *dest = POS(w1-w2);
  }
  else if (!sign1 && sign2) {
    overflow = false;
    if (w2 <= w1)
      *dest = NEG(w1-w2);
    else
      *dest = POS(w2-w1);
  }
  else if (!sign1 && !sign2) {
    overflow = (w1+w2) >= 1<<30;
    *dest = NEG(w1+w2);
  }
  return overflow;
}

// Return a pointer to:
// mix->A,              if I=0
// mix->Ii where i=I-1, if 1<=I<=6
// mix->X,              if I=7.
// Used to conveniently implement the LDx/STx/Jx/... families of functions
static word *Iaddr(int I, mix *mix) {
  return I == 0 ? &mix->A
       : I == 7 ? &mix->X : &mix->Is[I-1];
}

word getM(word instr, mix *mix) {
  // Manually cast the signed A part (13-bit) into a signed word (31-bit).
  word A = getA(instr);
  bool sign = (A >> 12) & 1;
  A = WITHSIGN(A & ONES(12), sign);
  // Add the contents of Iaddr
  byte I = getI(instr);
  if (I == 0)
    return A;
  addword(&A, *Iaddr(I, mix));
  return A;
}

bool subword(word *dest, word src) {
  return addword(dest, negword(src));
}

void mulword(word *destA, word *destX, word src) {
  uint64_t prod = (uint64_t)MAG(*destA) * MAG(src);
  bool prodsign = SIGN(*destA) == SIGN(src);
  *destA = WITHSIGN((prod >> 30) & ONES(30), prodsign);
  *destX = WITHSIGN(prod & ONES(30),         prodsign);
}

bool divword(word *destA, word *destX, word src) {
  if (MAG(src) == 0)
    return true;
  uint64_t w = COMBINE(*destA, *destX);
  word quot = w / MAG(src);
  word rem  = w % MAG(src);
  if ((uint64_t)quot * MAG(src) + rem < w)
    return true;
  bool quotsign = SIGN(*destA) == SIGN(src);
  *destA = WITHSIGN(quot, quotsign);
  *destX = WITHSIGN(rem,  quotsign);
  return false;
}

int compareword(word dest, word src) {
  bool sign1 = SIGN(dest);
  bool sign2 = SIGN(src);
  dest = MAG(dest);
  src = MAG(src);
  if (dest == 0 && src == 0) return 0;
  if (sign1 == sign2) {
    if (dest == src) return 0;
    if (dest > src) return sign1 ? 1 : -1;
    if (dest < src) return sign1 ? -1 : 1;
  }
  if (sign1 && !sign2) return 1;
  if (!sign1 && sign2) return -1;
}

void shiftleftword(word *dest, int M) {
  int shift_amt = 6*M >= 30 ? 30 : 6*M;  // Note that shifting a value by more than its width is UB.
  word w = MAG(*dest);
  w = (w << shift_amt) & ONES(30);
  *dest = WITHSIGN(w, SIGN(*dest));
}

void shiftrightword(word *dest, int M) {
  int shift_amt = (6*M >= 30 ? 30 : 6*M);
  word w = MAG(*dest);
  w = (w >> shift_amt) & ONES(30);
  *dest = WITHSIGN(w, SIGN(*dest));
}

void shiftleftwords(word *destA, word *destX, int M) {
  uint64_t w = COMBINE(*destA, *destX);
  int shift_amt = 6*M >= 60 ? 60 : 6*M;
  w = (w << shift_amt) & ONES(60);
  *destA = WITHSIGN((w >> 30) & ONES(30), SIGN(*destA));
  *destX = WITHSIGN(w & ONES(30),         SIGN(*destX));
}

void shiftrightwords(word *destA, word *destX, int M) {
  uint64_t w = ((*destA & ONES(30)) << 30) | (*destX & ONES(30));
  int shift_amt = 6*M >= 60 ? 60 : 6*M;
  w = (w >> shift_amt) & ONES(60);
  *destA = WITHSIGN((w >> 30) & ONES(30), SIGN(*destA));
  *destX = WITHSIGN(w & ONES(30),         SIGN(*destX));
}

void shiftleftcirc(word *destA, word *destX, int M) {
  uint64_t w = (MAG(*destA) << 30) | MAG(*destX);
  int shift_amt = (6*M) % 60;
  w = (w << shift_amt) | ((w >> 60-shift_amt) & ONES(60));
  w &= ONES(60);
  *destA = WITHSIGN(MAG(w >> 30), SIGN(*destA));
  *destX = WITHSIGN(MAG(w),       SIGN(*destX));
}

void shiftrightcirc(word *destA, word *destX, int M) {
  uint64_t w = (MAG(*destA) << 30) | MAG(*destX);
  int shift_amt = (6*M) % 60;
  w = (w >> shift_amt) | ((w & ONES(shift_amt)) << (60-shift_amt));
  w &= ONES(60);
  *destA = WITHSIGN(MAG(w >> 30), SIGN(*destA));
  *destX = WITHSIGN(MAG(w),       SIGN(*destX));
}

void wordtonum(word *destA, word *destX) {
  word w = MAG(*destA);
  int d1  = ((w >> 24) & ONES(6)) % 10;
  int d2  = ((w >> 18) & ONES(6)) % 10;
  int d3  = ((w >> 12) & ONES(6)) % 10;
  int d4  = ((w >>  6) & ONES(6)) % 10;
  int d5  = ( w        & ONES(6)) % 10;
  w = MAG(*destX);
  int d6  = ((w >> 24) & ONES(6)) % 10;
  int d7  = ((w >> 18) & ONES(6)) % 10;
  int d8  = ((w >> 12) & ONES(6)) % 10;
  int d9  = ((w >>  6) & ONES(6)) % 10;
  int d10 = ( w        & ONES(6)) % 10;
  uint64_t num = d10 + d9*10 + d8*100 + d7*1000 + d6*10000 +
    d5*100000 + d4*1000000 + d3*10000000 + d2*100000000 + d1*1000000000;
  num &= ONES(30);
  *destA = WITHSIGN((word)num, SIGN(*destA));
}

void numtochar(word *destA, word *destX) {
  int x = MAG(*destA);
  byte b1  = 30 + x / 1000000000; x %= 1000000000;
  byte b2  = 30 + x /  100000000; x %=  100000000;
  byte b3  = 30 + x /   10000000; x %=   10000000;
  byte b4  = 30 + x /    1000000; x %=    1000000;
  byte b5  = 30 + x /     100000; x %=     100000;
  byte b6  = 30 + x /      10000; x %=      10000;
  byte b7  = 30 + x /       1000; x %=       1000;
  byte b8  = 30 + x /        100; x %=        100;
  byte b9  = 30 + x /         10; x %=         10;
  byte b10 = 30 + x;
  *destA = WORD(SIGN(*destA), b1, b2, b3, b4, b5);
  *destX = WORD(SIGN(*destX), b6, b7, b8, b9, b10);
}

static unsigned char mixchr(byte b, unsigned char *extra) {
  *extra = '\0';
  switch (b) {
  case 0: return ' '; break;
  case 1: case 2: case 3: case 4: case 5:
  case 6: case 7: case 8: case 9:
    return 'A'+(b-1);
  case 10: *extra = 0x94; return 0xce;  // \u0394, Delta
  case 11: case 12: case 13: case 14: case 15:
  case 16: case 17: case 18: case 19:
    return 'J'+(b-11);
  case 20: *extra = 0xa3; return 0xce;  // \u03a3, Sigma
  case 21: *extra = 0xa0; return 0xce;  // \u03a0, Pi
  case 22: case 23: case 24: case 25: case 26:
  case 27: case 28: case 29:
    return 'S'+(b-22);
  case 30: case 31: case 32: case 33: case 34:
  case 35: case 36: case 37: case 38: case 39:
    return '0'+(b-30);
  case 40: return '.';
  case 41: return ',';
  case 42: return '(';
  case 43: return ')';
  case 44: return '+';
  case 45: return '-';
  case 46: return '*';
  case 47: return '/';
  case 48: return '=';
  case 49: return '$';
  case 50: return '<';
  case 51: return '>';
  case 52: return '@';
  case 53: return ';';
  case 54: return ':';
  case 55: return '\'';
  case 56: return 'a';
  case 57: return 'b';
  case 58: return 'c';
  case 59: return 'd';
  case 60: return 'e';
  case 61: return 'f';
  case 62: return 'g';
  case 63: return 'h';
  default: return '?';
  }
}

byte mixord(char c) {
  switch (c) {
  case ' ': return 0;
  // My custom non-unicode substitutes for Delta/Sigma/Pi
  case '!': return 10;
  case '[': return 20;
  case ']': return 21;
  case 'A': case 'B': case 'C': case 'D': case 'E':
  case 'F': case 'G': case 'H': case 'I':
    return 1+c-'A';
  case 'J': case 'K': case 'L': case 'M': case 'N':
  case 'O': case 'P': case 'Q': case 'R':
    return 11+c-'J';
  case 'S': case 'T': case 'U': case 'V': case 'W':
  case 'X': case 'Y': case 'Z':
    return 22+c-'S';
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    return 30+c-'0';
  case '.': return 40;
  case ',': return 41;
  case '(': return 42;
  case ')': return 43;
  case '+': return 44;
  case '-': return 45;
  case '*': return 46;
  case '/': return 47;
  case '=': return 48;
  case '$': return 49;
  case '<': return 50;
  case '>': return 51;
  case '@': return 52;
  case ';': return 53;
  case ':': return 54;
  case '\'': return 55;
  case 'a': return 56;
  case 'b': return 57;
  case 'c': return 58;
  case 'd': return 59;
  case 'e': return 60;
  case 'f': return 61;
  case 'g': return 62;
  case 'h': return 63;
  default: return 63;
  }
}

void initmix(mix *mix) {
  mix->done = false;
  mix->err = "";
  mix->PC = 0;
  memset(mix->exectimes, 0, 4000*sizeof(int));
  memset(mix->execcounts, 0, 4000*sizeof(int));

  mix->overflow = false;
  mix->cmp = 0;
  mix->A = POS(0);
  mix->X = POS(0);
  for (int i = 0; i < 6; i++)
    mix->Is[i] = POS(0);
  mix->J = POS(0);
  for (int i = 0; i < 4000; i++)
    mix->mem[i] = POS(0);
  mix->cardfile = NULL;
  for (int i = 0; i < 8; i++)
    mix->tapefiles[i] = NULL;

  for (int i = 0; i < 21; i++) {
    mix->iothreads[i].timer = 0;
    mix->iothreads[i].err = "";
  }
}

static void _write_char(unsigned char c, unsigned char extra, FILE *fp) {
  if (extra) {
    // I don't want to write a unicode Delta/Pi/Sigma to a file, so
    // instead I use the characters !/[/] respectively.
    if (extra == 0x94) fputc('!', fp);
    else if (extra == 0xa3) fputc('[', fp);
    else if (extra == 0xa0) fputc(']', fp);
  }
  else
    fputc(c, fp);
}

// Return false if there was an error carrying out the IO operation.
static bool execute_io(IOthread *iothread, mix *mix) {
  word M = iothread->M;
  word F = iothread->F;
  word C = iothread->C;

  // For each IO operation, the below code runs only *once*, when half
  // the specified time for the operation has elapsed.

#define CHECKADDR(i)                                        \
  if ((i)<0 || (i)>=4000) {                                 \
    iothread->err = "illegal address during IO operation";  \
    return false;                                           \
  }

  if (F == 16) {       // Card reader
    if (mix->cardfile == NULL) {
      iothread->err = "unspecified card file";
      return false;
    }
    for (int i = 0; i < 80; i++) {
      char c;
      if ((c = fgetc(mix->cardfile)) == EOF) {
	iothread->err = "unexpected EOF in middle of card";
	return false;
      }
      if (c == '\n') {
	i--;
	continue;
      }
      // Shift the character into the appropriate byte of the appropriate word.
      int pos = i%5 + 1;
      CHECKADDR(INT(M)+i/5)
      storeword(&mix->mem[INT(M)+i/5], mixord(c), pos*8 + pos);
    }
  }

  else if (F == 18) {  // Line printer
    // Each line has 120 characters (plus the trailing \n\0), a
    // character may need 2 bytes to encode (for the codepoints
    // 10=Delta, 20=Sigma, 21=Pi).
    unsigned char line[120*2+2];
    int c = 0;
    unsigned char extra;

#define ADDCHAR(b) line[c++] = mixchr((b), &extra);	\
    if (extra) line[c++] = extra;

    for (int i = 0; i < 24; i++) {
      CHECKADDR(INT(M)+i)
      word w = mix->mem[INT(M)+i];
      byte b1 = (w >> 24) & ONES(6); ADDCHAR(b1)
      byte b2 = (w >> 18) & ONES(6); ADDCHAR(b2)
      byte b3 = (w >> 12) & ONES(6); ADDCHAR(b3)
      byte b4 = (w >>  6) & ONES(6); ADDCHAR(b4)
      byte b5 =  w        & ONES(6); ADDCHAR(b5)
    }
    line[c++] = '\n';
    line[c++] = '\0';
    printf(line);
  }

  else if (0 <= F && F <= 7 && C == 36) {  // Tape IN
    if (mix->tapefiles[F] == NULL) {
      iothread->err = "unspecified tape file";
      return false;
    }
    for (int i = 0; i < 600; i++) {
      char c;
      if ((c = fgetc(mix->tapefiles[F])) == EOF) {
	iothread->err = "unexpected EOF in middle of tape";
	return false;
      }
      if (c == '\n') {
	i--;
	continue;
      }
      CHECKADDR(INT(M)+i/6)
      if (i%6 == 0) {
	// Load the word's sign
	if (c == '#')
	  mix->mem[INT(M)+i/6] = POS(0);
	else if (c == '~')
	  mix->mem[INT(M)+i/6] = NEG(0);
	else {
	  iothread->err = "invalid sign in tape, should be # or ~";
	  return false;
	}
      }
      else {
	// Shift the character into the appropriate byte of the appropriate word.
	int pos = i%6;
	storeword(&mix->mem[INT(M)+i/6], mixord(c), pos*8 + pos);
      }
    }
  }

  else if (0 <= F && F <= 7 && C == 37) {  // Tape OUT
    FILE *fp = mix->tapefiles[F];
    if (fp == NULL) {
      iothread->err = "unspecified tape file";
      return false;
    }
    unsigned char c, extra;
#define WRITECHAR(b) c = mixchr((b), &extra);	\
  _write_char(c, extra, fp);

    for (int i = 0; i < 100; i++) {
      CHECKADDR(INT(M)+i)
      word w = mix->mem[INT(M)+i];
      _write_char(SIGN(w) ? '#' : '~', '\0', fp);
      byte b1 = (w >> 24) & ONES(6); WRITECHAR(b1)
      byte b2 = (w >> 18) & ONES(6); WRITECHAR(b2)
      byte b3 = (w >> 12) & ONES(6); WRITECHAR(b3)
      byte b4 = (w >>  6) & ONES(6); WRITECHAR(b4)
      byte b5 =  w        & ONES(6); WRITECHAR(b5)
    }
    _write_char('\n', '\0', fp);
  }

  return true;
}

void onestep(mix *mix) {
  if (mix->done) return;

#define CHECKADDR(i)                  \
  if ((i)<0 || (i)>=4000) {           \
    mix->done = true;	              \
    mix->err = "illegal address";     \
    goto noadvance;                   \
  }

  CHECKADDR(mix->PC)
  word instr = mix->mem[mix->PC];
  byte C = getC(instr);
  byte F = getF(instr);
  byte I = getI(instr);
  word M = getM(instr, mix);
  // We don't want to evaluate V straight away, because INT(M) may not
  // be a valid address for instructions like ENTA
#define V() applyfield(mix->mem[INT(M)], F)

  // Update execution count/time
  int instrtime;
  // The instruction times for MOVE/IN/OUT/IOC are updated in their
  // respective branches, because they are variable.
  if (0 <= C && C <= 63)
    instrtime = instrtimes[C];
  int oldPC = mix->PC;

#define FIELDSPEC(C) if (!checkfieldspec(F)) {            \
    mix->done = true;                                     \
    mix->err = "invalid field for " #C;                   \
  }

  if (C == 1) {                                 // ADD
    FIELDSPEC("ADD")
    CHECKADDR(INT(M))
    mix->overflow = addword(&mix->A, V());
  }

  else if (C == 2) {                            // SUB
    FIELDSPEC("SUB")
    CHECKADDR(INT(M))
    mix->overflow = subword(&mix->A, V());
  }

  else if (C == 3) {                            // MUL
    FIELDSPEC("MUL")
    CHECKADDR(INT(M))
    mulword(&mix->A, &mix->X, V());
  }

  else if (C == 4) {                            // DIV
    FIELDSPEC("DIV")
    CHECKADDR(INT(M))
    mix->overflow = divword(&mix->A, &mix->X, V());
  }

  else if (C == 5) {
    if (F == 0)                                 // NUM
      wordtonum(&mix->A, &mix->X);
    else if (F == 1)                            // CHAR
      numtochar(&mix->A, &mix->X);
    else if (F == 2) {                          // HLT
      mix->done = true;
      mix->err = "";
    }
    else {
      mix->done = true;
      mix->err = "invalid field for SPECIAL";
    }
  }

  else if (C == 6) {
    if (F == 0)
      shiftleftword(&mix->A, INT(M));           // SLA
    else if (F == 1)
      shiftrightword(&mix->A, INT(M));          // SRA
    else if (F == 2)
      shiftleftwords(&mix->A, &mix->X, INT(M));   // SLAX
    else if (F == 3)
      shiftrightwords(&mix->A, &mix->X, INT(M));  // SRAX
    else if (F == 4)
      shiftleftcirc(&mix->A, &mix->X, INT(M));  // SLC
    else if (F == 5)
      shiftrightcirc(&mix->A, &mix->X, INT(M)); // SRC
    else {
      mix->done = true;
      mix->err = "invalid field for SHIFT";
    }
  }

  else if (C == 7) {                            // MOVE
    instrtime = 1 + 2*F;
    for (int i = 0; i < F; i++) {
      CHECKADDR(INT(M)+i)
      CHECKADDR(INT(mix->Is[0]))
      mix->mem[INT(mix->Is[0])] = mix->mem[INT(M)+i];
      mix->Is[0]++;
    }
  }

  else if (8 <= C && C <= 15) {                 // LDx
    FIELDSPEC("LDx")
    CHECKADDR(INT(M))
    loadword(Iaddr(C-8, mix), V());
  }

  else if (16 <= C && C <= 23) {                // LDxN
    FIELDSPEC("LDxN")
    CHECKADDR(INT(M))
    loadword(Iaddr(C-16, mix), negword(V()));
    // If sign is not part of F, make the word negative.
    if ((F>>3) & ONES(3) >= 1)
      *Iaddr(C-16, mix) = NEG(*Iaddr(C-16, mix));
  }

  else if (24 <= C && C <= 31) {                // STx
    FIELDSPEC("STx")
    CHECKADDR(INT(M))
    storeword(&mix->mem[INT(M)], *Iaddr(C-24, mix), F);
  }

  else if (C == 32) {                           // STJ
    FIELDSPEC("STJ")
    CHECKADDR(INT(M))
    storeword(&mix->mem[INT(M)], mix->J, F);
  }

  else if (C == 33) {                           // STZ
    FIELDSPEC("STZ")
    CHECKADDR(INT(M))
    storeword(&mix->mem[INT(M)], 0, F);
  }

  else if (C == 34) {                           // JBUS
    if (0 <= F <= 7 || F == 16 || F == 18) {
      if (mix->iothreads[F].timer > 0) {
	mix->J = POS(mix->PC+1);
	mix->PC = INT(M);
	goto noadvance;
      }
    }
    else {
      mix->done = true;
      mix->err = "invalid field for JBUS";
    }
  }

  else if (C == 35) {                           // IOC
    // TODO
  }

  else if (C == 36) {                           // IN
    if (0 <= F <= 7 || F == 16) {
      IOthread *iothread = &mix->iothreads[F];
      instrtime = 1 + iothread->timer;
      // If IO transmission hasn't happened, do it NOW and
      // immediately mark the operation as complete.
      // (Thus simulating a blocking operation.)
      if (iothread->timer > iothread->totaltime/2)
	execute_io(iothread, mix);
      // Reset the arguments.
      iothread->M = M;
      iothread->F = F;
      iothread->C = C;
      iothread->err = "";
      iothread->totaltime = mix->INtimes[F];
      iothread->timer = mix->INtimes[F];
    }
    else {
      mix->done = true;
      mix->err = "invalid input device for IN";
    }
  }

  else if (C == 37) {                           // OUT
    if (0 <= F <= 7 || F == 18) {
      IOthread *iothread = &mix->iothreads[F];
      instrtime = 1 + iothread->timer;
      // If IO transmission hasn't happened, do it NOW and
      // immediately mark the operation as complete.
      // (Thus simulating a blocking operation.)
      if (iothread->timer > iothread->totaltime/2)
	execute_io(iothread, mix);
      // Reset the arguments.
      iothread->M = M;
      iothread->F = F;
      iothread->C = C;
      iothread->err = "";
      iothread->totaltime = mix->OUTtimes[F];
      iothread->timer = mix->OUTtimes[F];
    }
    else {
      mix->done = true;
      mix->err = "invalid output device for OUT";
    }
  }

  else if (C == 38) {                           // JRED
    if (0 <= F <= 7 || F == 16 || F == 18) {
      if (mix->iothreads[F].timer == 0) {
	mix->J = POS(mix->PC+1);
	mix->PC = INT(M);
	goto noadvance;
      }
    }
    else {
      mix->done = true;
      mix->err = "invalid field for JRED";
    }
  }

  else if (C == 39) {
    if (F == 0 || F == 1 ||                     // JMP/JSJ
	(F == 2 && mix->overflow)  ||           // JOV
	(F == 3 && !mix->overflow) ||           // JNOV
	(F == 4 && mix->cmp < 0)   ||           // JL
	(F == 5 && mix->cmp == 0)  ||           // JE
	(F == 6 && mix->cmp > 0)   ||           // JG
	(F == 7 && mix->cmp >= 0)  ||           // JGE
	(F == 8 && mix->cmp != 0)  ||           // JNE
	(F == 9 && mix->cmp <= 0)) {            // JLE
      if (F != 1)
	mix->J = POS(mix->PC+1);
      CHECKADDR(INT(M))
      mix->PC = INT(M);
      goto noadvance;
    }
    else if (F >= 10) {
      mix->done = true;
      mix->err = "invalid field for JUMP";
    }
  }

  else if (40 <= C && C <= 47) {
    word w = *Iaddr(C-40, mix);
    if ((F == 0 && !SIGN(w) && MAG(w) > 0)   ||   // JxN
	(F == 1 && MAG(w) == 0)              ||   // JxZ
	(F == 2 && SIGN(w) && MAG(w) > 0)    ||   // JxP
	(F == 3 && (SIGN(w) || MAG(w) == 0)) ||   // JxNN
	(F == 4 && MAG(w) != 0)              ||   // JxNZ
	(F == 5 && (!SIGN(w) || MAG(w) == 0))) {  // JxNP
      mix->PC = INT(M);
      mix->J = POS(mix->PC+1);
      goto noadvance;
    }
    else if (F >= 6) {
      mix->done = true;
      mix->err = "invalid field for REGJUMP";
    }
  }

  else if (48 <= C && C <= 55) {
    if (F == 0)                                 // INCx
      mix->overflow = addword(Iaddr(C-48, mix), M);
    else if (F == 1)                            // DECx
      mix->overflow = subword(Iaddr(C-48, mix), M);
    else if (F == 2)                            // ENTx
      *Iaddr(C-48, mix) = M;
    else if (F == 3)                            // ENNx
      *Iaddr(C-48, mix) = negword(M);
    else {
      mix->done = true;
      mix->err = "invalid field for ADDROP";
    }
  }

  else if (56 <= C && C <= 63) {                // CMPx
    FIELDSPEC("CMP")
    CHECKADDR(INT(M))
    mix->cmp = compareword(applyfield(*Iaddr(C-56, mix), F), V());
  }

  else {
    mix->done = true;
    mix->err = "invalid instruction";
  }

  mix->PC++;

noadvance:
#define MORE_THAN_TWO_BYTES(w) (MAG(w)>>12 != 0)
  for (int i = 0; i < 6; i++) {
    if (MORE_THAN_TWO_BYTES(mix->Is[i])) {
      mix->done = true;
      if (i == 0)
	mix->err = "rI1 contains more than two bytes";
      else if (i == 1)
	mix->err = "rI2 contains more than two bytes";
      else if (i == 2)
	mix->err = "rI3 contains more than two bytes";
      else if (i == 3)
	mix->err = "rI4 contains more than two bytes";
      else if (i == 4)
	mix->err = "rI5 contains more than two bytes";
      else if (i == 5)
	mix->err = "rI6 contains more than two bytes";
    }
  }
  if (MORE_THAN_TWO_BYTES(mix->J)) {
    mix->done = true;
    mix->err = "rJ contains more than two bytes";
  }

  for (int i = 0; i < 21; i++) {
    IOthread *iothread = &mix->iothreads[i];
    // Execute IO operation exactly when half the specified time has
    // elapsed.
    if (iothread->timer <= 0) {
      iothread->timer = 0;
      continue;
    }
    if (iothread->timer > iothread->totaltime/2 &&
	iothread->timer - instrtime <= iothread->totaltime/2) {
      if (!execute_io(iothread, mix)) {
	mix->done = true;
	mix->err = iothread->err;
      }
    }
    iothread->timer -= instrtime;
  }

  if (mix->done) {
    // Flush the tape files
    for (int i = 0; i < 7; i++) {
      if (mix->tapefiles[i] != NULL)
	fflush(mix->tapefiles[i]);
    }
  }

  mix->execcounts[oldPC]++;
  mix->exectimes[oldPC] += instrtime;
}