# About
This is a brainfuck interpreter written in assembly. It was a university assignment for Computer Organization, and for people who wanted and extra challange, there was a competition held about who could make the fastest interpreter. I took part in the competition and tried to optimize my compiler a lot. I was told I did quite well in the competition, but segfaulted in one of the later rounds, disqualifying me from the tournament. I think the segfault was caused by a too small memory buffer for the JIT compiler, since I did not make a dynamically growing buffer because of a lack of time (I was working on the program alone because I couldn't find a teammate).

# How to run
* I developed my compiler in WSL Ubuntu, so start by getting that setup.
* Then compile using
``` bash
make
```
* Finally run using
``` bash
./brainfuck path/to/brainfuck/script
```
* To profile its performance, there is a test script, but I forgot how it works. Later on in the project I had switched to hyperfine anyways, so you can use that. For example:
``` bash
hyperfine "./brainfuck ./in/mandelbrot.b" --warmup 3 --runs 100
```

# File layout
Given to us as a template for the assignment:
 - main.s:
   This file contains the main function.
   It reads a file from a command line argument and passes it to your brainfuck implementation.

 - read_file.s:
   Holds a subroutine for reading the contents of a file.
   This subroutine is used by the main function in main.s.

 - brainfuck.s:
   This is where you should put your brainfuck implementation.
   In it you should define a `brainfuck` subroutine that takes
   a single argument: a string holding the code to execute.

 - Makefile:
   A file containing compilation information. If you have a working make,
   you can compile the code in this directory by simply running the command `make`.

I myself added a main_test.s for easier development and testing performance, but I forgot how it works and how to use it.