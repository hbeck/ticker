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
    * explicitly: `sbt "run-main Program --program src/test/resources/test.rules --reasoner Clingo"`
* Executing as jar:
    * Building the jar with SBT: `sbt assembly` 
    * Running it via JAVA: `java -jar target/scala-2.12/ticker-assembly-1.0.jar --program src/test/resources/test.rules`

The running engine can be terminated by pressing CTRL-C.

All parameters and options can be printed by calling the executable with `--help`.

Most useful options include:

* `--program <file>` for the program which should be used in the engine
* `--reasoner <type>` for the Reasoning Engine 
    * Ticker as incremental reasoner
    * Clingo for one shot solving
* `--timeunit <unit>` for the engine timeunit (time based interpretation of '1 tick'), e.g. `1sec` or `200millis`
* `--outputEvery <config>` defines the output behavior of the engine: 
    * `diff` for every model change
    * `signal` for changes at every input signal (push based)
    * `time` for changes at every time/tick change
    * `<number>signals` for updates every <number> input signals
    * `<value><time unit>` for updates every specified time units
* `--inputType <type 1>,<type ...>` specifies multiple input sources for signals
    * `StdIn` for reading from standard input
    * `socket:<port>` for reading from a socket with a specified port
* `--outputType <type 1>,<type ...>` specifies multiple output sinks for models
    * `StdOut` for printing models to standard output
    * `socket:<port>` for writing model changes to a port

