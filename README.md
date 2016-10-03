Haskell Brainfuck Interpreter
=============================

This is a little brainfuck interpreter I wrote in haskell as an
exercise. The run function takes in 2 strings to use as the program
and stdin respectively, and returns stdout.

Example Programs
----------------
 - Adds 1 to stdin chars, ends when . is encountered:
   - "+[,[>+>+<<-]++++++[>-------<-]>----[<+>[-]>+.[-]<]<]"
 - Hello, world!:
   - "+++++++++[>++++++++<-]>.<+++++++[>++++<-]>+.+++++++..+++.<<++++[>++++++++<-]>.>++++++++.--------.+++.------.--------.<+.<++++++++++."
