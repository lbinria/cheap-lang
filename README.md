# Cheap-lang: the cheap chip-8 language

Here a "compiler" for the programming language "cheap" for Chip-8.

# Pre-build binaries

In the future you probably find some releases...

# Prerequisite

 - Install opam
 - Install dune
 - Install menhir with opam

# Run with dune

This command will compile and run `cheap-lang` using dune:

`dune exec cheap-lang {sprite_file.sprites} {program.cheap}`

# The cheap language tutorial

## Introduction

 - Compilation or transpilation ?
 - Limited data memory
 - Limited program memory
 - No types, all is int !
 - Registers
 - Variable are aliases that you bind with register
 - No real function, just sub routines ! as chip-8 has limited stack
 - Conditional / While statements content as subroutines
 - Main loops
 - Clocks !

 Chip-8 manipulate only one type of data: int. In the cheap language there is no mechanism to manipulate other types. So as chip-8 assembly all values / variables are int. Int are encoded on 8 bits value and is range is $2^8 = 255, [0-255]$.
Some values are encoded on 12 bit int and their range is $2^12 = 4096, [0-4096]$.

## Basic instructions

TODO

## Control structures

TODO

## Subroutines

TODO

## Cost (in size) table of cheap instructions

Below you can find the table that references all cheap language instructions and their size in the program when there are compiled.

| Cheap instruction | Number of chip-8 instructions | Size (bytes) | Notes |
| ----------- | ----------- | ----------- | ----------- |
| `while (condition) { ... }` | 3 + $N_i$ | 6 + $N_i$ * 2 |  Condition / Call / Return + Body instructions |

TODO complete