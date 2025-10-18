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

 - No types, all is int !
 - Limited data memory
 - Limited program memory
 - Variable are aliases that you bind with register
 - No real function, just sub routines ! as chip-8 has limited stack
 - Conditional / While statements content as subroutines
 - Main loop