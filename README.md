# AOC Template for Common Lisp
A template for Advent Of Code written in and for Common Lisp

## Setup
Make sure you have installed:
- A Common Lisp REPL (most ideally [SBCL](http://www.sbcl.org/))
- [quicklisp](https://www.quicklisp.org/beta/)

I also use [SLIME](https://common-lisp.net/project/slime/) for easier loading of files

## Usage
Start your LISP REPL in the template directory, type `(load "main")` in the REPL to load the template.
`(ld <day-number>)` to load that day into the REPL, this will attempt to run the parser and the solutions
to both parts of the problem. If successful, the results will be stored in a day structure that will be shown
and can be accessed by typing `day#` in the REPL, where `#` is the day number. You can compare the example
solutions with yours by using the `(test-days)` function. Other functions with full descriptions can be seen by
typing `(help)` in the REPL.

### Input files
The input files for each day go into the "input" folder and are named "##.txt" where ## is the day number
in two figures (e.g. 01 for day 1). You can also add the examples for each day in the "input/test" folder
