# Starting the remote/interactive clingo solve

Tested with Clingo 5.1.0 on Mac

## Starting the server first
python server.py

## Connecting the client to the server

./clingo client.lp client-py.lp

## sending commands to the server

'tick' increases the tick/time in the clingo program
'solve' instructs clingo to start solving
'signal <atom>' sends a signal to clingo


