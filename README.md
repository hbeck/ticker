# STEEN - Stream reasoning engine

Provides an engine for executing stream reasoning tasks in a LARS-influenced Syntax.

## Requirements
* Requires Scala 2.11.7 or higher
* Building should be done by using SBT (Scala Built Tools) with version 0.13 or higher.
* Packaging currently done over Intelli-J JAR-Packing

## Writing a program
Current version of the program parser supports the following notation:

* atoms start with a letter
* 1 Rule per line: `a :- b`
* Multiple clauses separated by `,`: `a :- b, c, d`
* Negation via `not`: `a :- b, not c`
* Arguments via `a(1, 2)`: `a :- b(1), c(a, 2)`
** If the argument is convertable into a number it is treated as such, otherwise it is a string
* Window operators via fix defined format: `w_d_10_a(a)`
** `w_`: prefix for window atom
** 'd_': Operator (d-Diamond, b-Box)
** '10_': Window-Size in default engine-units
*** WindowSize in different Units: '10s' for 10 seconds, '10m' for 10 minutes, '10ms' for 10 milli-seconds
** 'a': needed to make window name unique
*** '(a)': parameter of the predicate

### Sample Program

```
a :- w_d_10s_x(x)
b :- w_b_5s_y(y)
c :- a, not b
```

## Running a Program

Run the program by executing `java -cp steen.jar Program --program <program file> --evaluationType Tms` in a command line.

Replace <program file> with a path to a file containing the program in the LARS-based Syntax

The engine can be terminated by pressing CTRL-C