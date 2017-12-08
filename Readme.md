# Ticker - A stream reasoning engine

Provides an engine for executing stream reasoning tasks in a LARS-influenced Syntax.

## Requirements
* Requires Scala 2.12.2 or higher
* Building should be done by using SBT (Scala Built Tools) with version 0.13 or higher.

## Writing a program
Current version of the program parser supports the following notation:

TODO: Eddie syntax description

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
    
## Running Ticker

To execute ticker as a separate program and stream input signals from an external source,
 the following possibilities are currently implemented.

### Receive input from Std-In and output to Std-Out

Ticker can be started to read input data from Std-Input and write it into Std-Ouput.

```sh
tail -F input.txt | java -jar target/scala-2.12/ticker-assembly-1.0.jar --program src/test/resources/test.rules >> out.txt
```

Reads data from a `input.txt` file and writes all model updates into 'out.txt'

### Connect input and output streams via sockets

Ticker can be configured to receive input signals from a plain system socket over a specified port. 

#### Listen to a (local) socket

Prior to starting ticker a socket must be opened: `nc -l 9999`. 
With this connection input values are written to ticker.

#### Staring ticker

Ticker can then be started with the following parameters, 
important are the correct socket socket ports `9999` and `9998`.
 
```sh
java -jar target/scala-2.12/ticker-assembly-1.0.jar \
                             --program src/test/resources/test.rules \
                             --reasoner Clingo \
                             --inputType socket:9999 \
                             --timeunit 1s \
                             --outputEvery diff \
                             --outputType socket:9998
```                             
#### Connect to a (local) socket

Connecting to a socket for receiving output values and model computations: `nc localhost 9999` 

## Creating a Ticker build

First `sbt compile` should be executed. 

Then a new jar-package can be created with `sbt assembly`.