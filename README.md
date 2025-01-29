# Module for Manipulating the Mythical Machine MIX

This is a MIX emulator I wrote to follow along the code examples in Knuth's _The Art of Computer Programming_. Actually, there is an emulator, an assembler and a debugger called the _MIX Management Module_ (which is also used to run MIXAL programs).

Some examples of MIX programs I wrote can be found [here](https://github.com/greysome/mixstuff).

## Installation

There are two executables: `mmm` the MIX Management Module and `test`, which just runs a series of asserts to sanity-check that the emulator and assembler work as intended. They can be built via `make` and `make test` respectively. The only dependency is the C standard library, and I compile with C17 (older versions of C will probably work too).

## Basic usage

```
> mmm program.mixal
Loaded MIXAL file program.mixal
MIX Management Module, by wyan
Type h for help
>>
```

The most basic task is to run a program as-is. This can be done by typing `g` in the prompt. Note that after the program is done, `g` does nothing; to run again, send `l` first to reset the MIX machine. There also commands to help with debugging; here are some useful ones:

|  Command |   |
| ------------ | ------------ |
| `v`,`v1000`,`v1000-2000`,`v.LABEL` | View memory |
| `b1000`, `b.LABEL` | Run till breakpoint |
| `g`, `g2`, `g+` | Run with different levels of tracing |
| `r` | View register contents |
| `t` | View timing statistics |

Also, many MIXAL programs involve I/O, and we need to specify where to read the input and write the output. I/O devices such as cards and tapes are represented as plain text files, and each file is essentially a sequence of words encoded with MIX's character set. (For specifics, look at "Card format" and "Tape format".)

If the program reads input from cards, we can specify it as a command-line argument, or inside the mmm prompt:

```
> mmm program.mixal input.cards
Loaded MIXAL file program.mixal
Loaded card file input.cards
MIX Management Module, by wyan
Type h for help
>> @input2.cards
Loaded card file input2.cards
```

If tape n is used for either input or output, then it needs to be specified in the prompt:

```
>> #n<tapefile>
Loaded tape file <tapefile>
```
## Card format

A card file (`.cards`) consists of a series of words, and a word is a sequence of 5 bytes encoded in MIX's character set, with two modifications:
- The characters $\Delta$, $\Sigma$ and $\Pi$ (corresponding to codes 10,20,21) are replaced with `!`, `[` and `]`, for compatibility with ASCII.
- I've added the characters `abcdefgh` for the codes 56-63. Thus, every positive word can be input into a MIX program via cards -- in particular we can load MIX programs!

Note that these are non-standard changes which are only supported by my implementation.

## Tape format

A tape file (`.tape`) consists of a series of words. Unlike cards however, the sign is also stored at the start of each word: `#` for positive and `~` for negative (because `+` and `-` are already used in the character set. The encoding of bytes is exactly the same as in cards.