#ifndef _EMULATOR_H
#define _EMULATOR_H
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

int max(int a, int b);

// A byte in MIX holds 64 values, hence is actually 6-bit.
// The first 2 bits are unused.
typedef uint8_t byte;
// A word consists of 5 bytes and a sign, hence 31-bit.
// The first bit is unused.
// This type is also used for index and jump registers out of
// convenience, even though they are supposed to have 2 bytes.
typedef uint32_t word;

// Useful macros for manipulating words
#define ONES(n) (((uint64_t)1<<(n)) - 1)
#define MAG(w) ((w) & ONES(30))
#define SIGN(w) (((w)>>30) & 1)
#define POS(w) ((w) | (1<<30))
#define NEG(w) MAG(w)
#define WITHSIGN(w,s) ((s ? POS(w) : NEG(w)))
#define COMBINE(w,v) ((MAG(w) << 30) | MAG(v))
#define INT(w) (SIGN(w) ? MAG(w) : -MAG(w))

typedef struct {
  bool done;
  int PC;         // Program counter
  // Keep track of the execution counts and times of each memory cell.
  int execcounts[4000];
  int exectimes[4000];

  bool overflow;  // Overflow toggle
  int cmp;        // Comparison indicator
  word A, X;
  word Is[6];     // Index registers
  word J;         // Jump register
  // (Technically the I and J registers only have 2 bytes, but it is
  //  convenient to reuse the word type.)
  word mem[4000];
} mix;

// Return a pointer to:
// mix->A,              if I=0
// mix->Ii where i=I-1, if 1<=I<=6
// mix->X,              if I=7.
// Used to conveniently implement the LDx/STx/Jx/... families of functions
word *Ix(int I, mix *mix);

// Construct a 13-bit value consisting of a sign and 2 bytes.
// The 2 bytes store the magnitude of x, i.e. not using two's
// complement.
// Used for the A field of an instruction.
word ADDR(int x);
// Construct a 31-bit word from the given sign and bytes.
word WORD(bool sign, byte b1, byte b2, byte b3, byte b4, byte b5);
// Construct a 31-bit instruction word from the given fields.
// A should be a 13-bit signed value created by ADDR().
word INSTR(word A, byte I, byte F, byte C);

word getA(word instr);
byte getI(word instr);
byte getF(word instr);
byte getC(word instr);
word getM(word instr, mix *mix);
word getV(word instr, mix *mix);



// Return the portion of w specified by the field F.
word applyfield(word w, byte F);

void loadword(word *dest, word src, byte F);
void storeword(word *dest, word src, byte F);
word negword(word w);
bool addword(word *dest, word src, byte F);
bool subword(word *dest, word src, byte F);
void mulword(word *destA, word *destX, word src, byte F);
bool divword(word *destA, word *destX, word src, byte F);
int compareword(word dest, word src, byte F);

void wordtonum(word *destA, word *destX);
void numtochar(word *destA, word *destX);

void shiftleftword(word *dest, int M);
void shiftrightword(word *dest, int M);
void shiftleftwords(word *destA, word *destX, int M);
void shiftrightwords(word *destA, word *destX, int M);
void shiftleftcirc(word *destA, word *destX, int M);
void shiftrightcirc(word *destA, word *destX, int M);



void initmix(mix *mix);
void onestep(mix *mix);
#endif