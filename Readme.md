# Ticker - A stream reasoning engine

* paper: https://arxiv.org/abs/1707.05304

## Requirements
* Requires Scala 2.12.2 or higher
* Building should be done by using SBT (Scala Built Tools) with version 0.13 or higher.

## Writing a program
Current version of the program parser supports the following notation:

### Sample Program

The following programs tests whether in the last 10 seconds more than
two cars have been recorded. The first line infers `moreThanTwo` if a
derived atom `third` is associated with (at least) one of the last 10
seconds. The second rule considers the tuple-based (count-based)
windows of the last three, respectively two atoms. If there is a
`rec(C)` atom for a `car` identifier `C` among the last three that is
not within the last two, it is the third (in the past). In this case
we associate the time point `T` of its appearence in the stream with
auxiliary atom `third`.

```
moreThanTwo :- third [10 sec].
@T third :- car(C), @T rec(C) [3 #], not rec(C) [2 #].
car(1). car(2). car(3). car(4).
```

Variables appearing in the scope of a window (as in the second line)
must be guarded for pre-grounding. (Hence the car facts in the last
line.)

## Executing a program with the engine

There are multiple ways to execute a program:

* Using SBT from a shell: `sbt "run --program src/test/resources/program.lars"`
    * explicitly: `sbt "run-main Program --program src/test/resources/program.lars"`
* Executing as jar:
    * Building the jar with SBT: `sbt assembly` 
    * Running it via JAVA: `java -jar target/scala-2.12/ticker-assembly-1.0.jar --program src/test/resources/program.lars`

The running engine can be terminated by pressing CTRL-C.

All parameters and options can be printed by calling the executable with `--help`.

Mandatory parameter:

* `-p --program <file>` for the program which should be used in the engine

(Either selector `-p` or `--program` can be used.)

Optional parameters:

* `-r --reasoner [ incremental | clingo ]` for the reasoning mode
* `-c --clock <int><timeunit>` for the duration of 1 time point, where `<timeunit> ::= ms | s | sec | min | h`
* `-e --outputEvery [ change | signal | time | <int>signals | <int><timeunit> ]` for specifying when output is written:
    * `change`: filtered model changed
    * `signal`: new signal streamed in (push-based)
    * `time`: a time point passed by (pull-based)
    * `<int>signals`: given number signals streamed in (generalized push-based)
    * `<int><timeunit>`: specified time passed by (pull-based)
* `-i --input <source>,<source>,...`
    * `<source>: stdin | socket:<int>`
* `-o --output <sink>,<sink>,...`
    * `<sink>: stdout | socket:<int>`

default: `-r incremental -f none -c 1s -e change -i stdin -o stdout`
    
## Running Ticker

To execute ticker as a separate program and stream input signals from an external source,
 the following possibilities are currently implemented.

### Receive input from Std-In and output to Std-Out

Ticker can be started to read input data from Std-Input and write it into Std-Ouput.

```sh
tail -F input.txt | java -jar target/scala-2.12/ticker-assembly-1.0.jar --program src/test/resources/program.lars >> out.txt
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
                             --program src/test/resources/program.lars \
                             --reasoner incremental \
                             --input socket:9999 \
                             --clock 1s \
                             --outputEvery change \
                             --output socket:9998
```                             
#### Connect to a (local) socket

Connecting to a socket for receiving output values and model computations: `nc localhost 9999` 

## Creating a Ticker build

First `sbt compile` should be executed. 

Then a new jar-package can be created with `sbt assembly`.