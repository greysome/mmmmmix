#include "emulator.h"

int max(int a, int b) { return a >= b ? a : b; }

word *Ix(int I, mix *mix) {
  return I == 0 ? &mix->A
       : I == 7 ? &mix->X : &mix->Is[I-1];
}

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

word applyfield(word w, byte F) {
  int start = F/8, end = F%8;
  assert(0 <= start && start <= 5);
  assert(start <= end && end <= 5);
  bool sign = (w >> 30) & 1;
  word v = w >> 6*(5-end);  // Last byte of v = end byte of w
  v &= ONES(6 * (end - max(start,1) + 1));  // Keep the desired bytes based on F
  if (start == 0)
    return WITHSIGN(v, sign);
  else
    return POS(v);
}

void loadword(word *dest, word src, byte F) {
  *dest = applyfield(src, F);
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

bool addword(word *dest, word src, byte F) {
  src = applyfield(src, F);
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

word getM(word instr, mix *mix) {
  // Manually cast the signed A part (13-bit) into a signed word (31-bit).
  word A = getA(instr);
  bool sign = (A >> 12) & 1;
  A = WITHSIGN(A & ONES(12), sign);
  // Add the contents of Ix
  byte I = getI(instr);
  if (I == 0)
    return A;
  addword(&A, *Ix(I, mix), 5);
  return A;
}

word getV(word instr, mix *mix) {
  return mix->mem[INT(getM(instr, mix))];
}

bool subword(word *dest, word src, byte F) {
  return addword(dest, negword(src), F);
}

void mulword(word *destA, word *destX, word src, byte F) {
  src = applyfield(src, F);
  uint64_t prod = (uint64_t)MAG(*destA) * MAG(src);
  bool prodsign = SIGN(*destA) == SIGN(src);
  *destA = WITHSIGN((prod >> 30) & ONES(30), prodsign);
  *destX = WITHSIGN(prod & ONES(30),         prodsign);
}

bool divword(word *destA, word *destX, word src, byte F) {
  src = applyfield(src, F);
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

int compareword(word dest, word src, byte F) {
  word w1 = applyfield(dest, F);
  word w2 = applyfield(src, F);
  bool sign1 = SIGN(w1);
  bool sign2 = SIGN(w2);
  w1 = MAG(w1);
  w2 = MAG(w2);
  if (w1 == 0 && w2 == 0) return 0;
  if (sign1 == sign2) {
    if (w1 == w2) return 0;
    if (w1 > w2) return sign1 ? 1 : -1;
    if (w1 < w2) return sign1 ? -1 : 1;
  }
  if (sign1 && !sign2) return 1;
  if (!sign1 && sign2) return -1;
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

void printbyte(byte b) {
  assert(b <= 55);
  switch (b) {
  case 0: putchar(' '); break;

  case 1: case 2: case 3: case 4: case 5:
  case 6: case 7: case 8: case 9:
    putchar('A'+(b-1));
    break;

  case 10: printf("\u0394"); break;

  case 11: case 12: case 13: case 14: case 15:
  case 16: case 17: case 18: case 19:
    putchar('J'+(b-11));
    break;

  case 20: printf("\u03a3"); break;
  case 21: printf("\u03a0"); break;

  case 22: case 23: case 24: case 25: case 26:
  case 27: case 28: case 29:
    putchar('S'+(b-22));
    break;

  case 30: case 31: case 32: case 33: case 34:
  case 35: case 36: case 37: case 38: case 39:
    putchar('0'+(b-30));
    break;

  case 40: putchar('.'); break;
  case 41: putchar(','); break;
  case 42: putchar('('); break;
  case 43: putchar(')'); break;
  case 44: putchar('+'); break;
  case 45: putchar('-'); break;
  case 46: putchar('*'); break;
  case 47: putchar('/'); break;
  case 48: putchar('='); break;
  case 49: putchar('$'); break;
  case 50: putchar('<'); break;
  case 51: putchar('>'); break;
  case 52: putchar('@'); break;
  case 53: putchar(';'); break;
  case 54: putchar(':'); break;
  case 55: putchar('\''); break;
  }
}

void initmix(mix *mix) {
  mix->done = false;
  mix->PC = 0;
  memset(mix->execcounts, 0, 4000*sizeof(int));
  mix->overflow = false;
  mix->cmp = 0;
  mix->A = POS(0);
  mix->X = POS(0);
  for (int i = 0; i < 6; i++)
    mix->Is[i] = POS(0);
  mix->J = POS(0);
  memset(mix->mem, 0, 4000*sizeof(word));
  memset(mix->exectimes, 0, 4000*sizeof(int));
}

void onestep(mix *mix) {
  if (mix->done) return;

  word instr = mix->mem[mix->PC];
  byte C = getC(instr);
  byte F = getF(instr);
  byte I = getI(instr);
  word M = getM(instr,mix);
#define V() getV(instr,mix)
  mix->execcounts[mix->PC]++;
#define LOGTIME(t) mix->exectimes[mix->PC] += (t)

  if (C == 0)
    LOGTIME(1);

  else if (C == 1) {                            // ADD
    LOGTIME(2);
    mix->overflow = addword(&mix->A, V(), F);
  }

  else if (C == 2) {                            // SUB
    LOGTIME(2);
    mix->overflow = subword(&mix->A, V(), F);
  }

  else if (C == 3) {                            // MUL
    LOGTIME(10);
    mulword(&mix->A, &mix->X, V(), F);
  }

  else if (C == 4) {                            // DIV
    LOGTIME(12);
    mix->overflow = divword(&mix->A, &mix->X, V(), F);
  }

  else if (C == 5) {
    if (F == 0) {                               // NUM
      LOGTIME(10);
      wordtonum(&mix->A, &mix->X);
    }
    else if (F == 1) {                          // CHAR
      LOGTIME(10);
      numtochar(&mix->A, &mix->X);
    }
    else if (F == 2)                            // HLT
      mix->done = true;
  }

  else if (C == 6) {
    LOGTIME(2);
    if (F == 0)
      shiftleftword(&mix->A, INT(M));                // SLA
    else if (F == 1)
      shiftrightword(&mix->A, INT(M));               // SRA
    else if (F == 2)
      shiftleftwords(&mix->A, &mix->X, INT(M));      // SLAX
    else if (F == 3)
      shiftrightwords(&mix->A, &mix->X, INT(M));     // SRAX
    else if (F == 4)
      shiftleftcirc(&mix->A, &mix->X, INT(M));       // SLC
    else if (F == 5)
      shiftrightcirc(&mix->A, &mix->X, INT(M));      // SRC
  }

  else if (C == 7) {                            // MOVE
    LOGTIME(1 + 2*F);
    for (int i = 0; i < F; i++)
      mix->mem[mix->Is[0]++] = mix->mem[INT(M)+i];
  }

  else if (8 <= C && C <= 15) {                 // LDx
    LOGTIME(2);
    loadword(Ix(C-8, mix), V(), F);
  }

  else if (16 <= C && C <= 23) {                // LDxN
    LOGTIME(2);
    loadword(Ix(C-8, mix), negword(V()), F);
  }

  else if (24 <= C && C <= 31) {                // STx
    LOGTIME(2);
    storeword(&mix->mem[INT(M)], *Ix(C-24, mix), F);
  }

  else if (C == 32) {                           // STJ
    LOGTIME(2);
    storeword(&mix->mem[INT(M)], mix->J, F);
  }

  else if (C == 33) {                           // STZ
    LOGTIME(2);
    storeword(&mix->mem[INT(M)], 0, F);
  }

  else if (C == 34) {
  }

  else if (C == 35) {
  }

  else if (C == 36) {
  }

  else if (C == 37) {                           // OUT
    LOGTIME(1);
    if (F == 18) {
      for (int i = 0; i < 24; i++) {
	word w = mix->mem[INT(M)+i];
	byte b1 = (w >> 24) & ONES(6); printbyte(b1);
	byte b2 = (w >> 18) & ONES(6); printbyte(b2);
	byte b3 = (w >> 12) & ONES(6); printbyte(b3);
	byte b4 = (w >>  6) & ONES(6); printbyte(b4);
	byte b5 =  w        & ONES(6); printbyte(b5);
      }
      putchar('\n');
    }
  }

  else if (C == 38) {
  }

  else if (C == 39) {               
    LOGTIME(1);
    if (F == 0 || F == 1 ||                     // JMP/JSJ
	(F == 2 && mix->overflow)  ||           // JOV
	(F == 3 && !mix->overflow) ||           // JNOV
	(F == 4 && mix->cmp < 0)   ||           // JL
	(F == 5 && mix->cmp == 0)  ||           // JE
	(F == 6 && mix->cmp > 0)   ||           // JG
	(F == 7 && mix->cmp >= 0)  ||           // JGE
	(F == 8 && mix->cmp != 0)  ||           // JNE
	(F == 9 && mix->cmp <= 0)) {            // JLE
      mix->PC = INT(M);
      if (F != 2)
	mix->J = mix->PC+1;
      goto noadvance;
    }
  }

  else if (40 <= C && C <= 47) {
    LOGTIME(1);
    word w = *Ix(C-40, mix);
    if ((F == 0 && !SIGN(w) && MAG(w) > 0) ||   // JxN
	(F == 1 && MAG(w) == 0)            ||   // JxZ
	(F == 2 && SIGN(w) && MAG(w) > 0)  ||   // JxP
	(F == 3 && (SIGN(w) || MAG(w)==0)) ||   // JxNN
	(F == 4 && MAG(w) != 0)            ||   // JxNZ
	(F == 5 && (!SIGN(w) || MAG(w)==0))) {  // JxNP
      mix->PC = INT(M);
      mix->J = mix->PC+1;
      goto noadvance;
    }
  }

  else if (48 <= C && C <= 55) {
    LOGTIME(1);
    if (F == 0)                                 // INCx
      mix->overflow = addword(Ix(C-48, mix), M, 5);
    else if (F == 1)                            // DECx
      mix->overflow = subword(Ix(C-48, mix), M, 5);
    else if (F == 2)                            // ENTx
      *Ix(C-48, mix) = M;
    else if (F == 3)                            // ENNx
      *Ix(C-48, mix) = negword(M);
  } 

  else if (56 <= C && C <= 63) {                // CMPx
    LOGTIME(1);
    mix->cmp = compareword(*Ix(C-56, mix), V(), F);
  }

  mix->PC++;

noadvance:
#define MORE_THAN_TWO_BYTES(w) (MAG(w)>>12 != 0)
  for (int i = 0; i < 6; i++) {
    if (MORE_THAN_TWO_BYTES(mix->Is[i])) {
      //printf("ERROR: rI%d contains more than two bytes\n", i+1);
    }
  }
  if (MORE_THAN_TWO_BYTES(mix->J)) {}
    //printf("ERROR: rJ contains more than two bytes\n");
}