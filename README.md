# STEEN - Stream reasoning engine

Provides an engine for executing stream reasoning tasks in a LARS-influenced Syntax.

## Requirements
* Requires Scala 2.11.7 or higher
* Building should be done by using SBT (Scala Built Tools) with version 0.13 or higher.
* Packaging currently done over Intelli-J JAR-Packing

## Writing a program
Current version of the program parser supports the following notation:

* atoms start with a (lower case) letter
* 1 Rule per line: `a :- b`
* Multiple clauses separated by `,`: `a :- b, c, d`
* Negation via `not`: `a :- b, not c, not d`
* Arguments via `a(1, 2)`: `a :- b(1), not c(a, 2)`
* * If the argument is convertable into a number it is treated as such, otherwise it is a string
* Window operators via fix defined format: `w_10_d_a(b)`
* * `w_`: prefix for window atom
* * `10_`: Window-Size in default engine-units
* * * WindowSize in different types
* * * No postfix: `10` -> Sliding Time Window with 10 units in the default engine time unit
* * * Sliding-Time-Window Postfixes:  `10d` for 10 days, `10h` for 10 hours, `10min` for 10 minutes, `10s` for 10 seconds, `10ms` for 10 milli-seconds
* * * Tuple-Based-Window Postfix: `10t` for last 10 input tuple
* * `d_`: Operator (d-Diamond, b-Box)
* * `a`: name of the predicate
* * `(b)`: optional argument of the window atom


### Sample Program

```
a :- w_10s_d_x
b :- w_5t_b_y(1)
c :- a, not b
```

## Running a Program

Run the program by executing `java -cp steen.jar Program --program <program file> --evaluationType Tms` in a command line.

Replace <program file> with a path to a file containing the program in the LARS-based Syntax

The engine can be terminated by pressing CTRL-C