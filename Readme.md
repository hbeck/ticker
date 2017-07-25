# Ticker - A stream reasoning engine

Provides an engine for executing stream reasoning tasks in a LARS-influenced Syntax.

## Requirements
* Requires Scala 2.12.2 or higher
* Building should be done by using SBT (Scala Built Tools) with version 0.13 or higher.

## Writing a program
Current version of the program parser supports the following notation:

TODO: Eddie syntax

### Sample Program

```
a :- x in [10 sec].
b :- y(1) always in [5 #].
c :-  a, not b.
```

## Executing a program with the engine

There are multiple ways to execute a program:

* Using SBT from a shell: `sbt "run --program src/test/resources/test.rules --reasoner Clingo"`
** `sbt "run-main Program --program src/test/resources/test.rules --reasoner Clingo"`
* Building an jar with SBT: `sbt assembly` and running it via JAVA: `java -jar target/scala-2.12/ticker-assembly-1.0.jar --program src/test/resources/test.rules`

The engine can be terminated by pressing CTRL-C

