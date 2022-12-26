# Jack Compiler

Compiler for the Jack Lanaguage from [Nand2Tetris](https://www.nand2tetris.org/).

The Jack compiler is almost always an LL(1) compiler except for in a few rare
LL(2) cases.

The Jack compiler is a two-tiered compiler with the first step compiling source code
to VM Code (very simlar to Java bytecode) and the second step translating the VM Code.

# Jack Language

The Jack language is the language developed for Nand2Tetris. It has a very simple
Java like syntax.
