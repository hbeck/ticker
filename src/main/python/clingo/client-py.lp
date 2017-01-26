
#script (python) 

import socket
import sys
import os
import errno
import clingo
# from gringo import Fun, SolveResult, Model
from clingo import SolveResult,Model

class Receiver:
    def __init__(self, conn):
        self.conn = conn
        self.data = bytearray()
    def readline(self):
        pos = self.data.find("\n")
        while pos < 0:
            while True:
                try: self.data.extend(self.conn.recv(4096))
                except socket.error as (code, msg):
                    if code != errno.EINTR: raise
                else: break
            pos = self. data.find("\n")
        msg = self.data[:pos]
        self.data = self.data[pos+1:]
        return msg

class States:
    SOLVE = 1
    IDLE  = 2

def printModel(model):
    # print "Model ", dir(model)
    print "Atoms ", model.symbols(True)

def on_model(conn, model):
    printModel(model)
    conn.sendall("Answer: " + str(model) + "\n")

def finished(conn, ret, interrupted):
    conn.sendall("finish:" + str(ret) + (":INTERRUPTED" if interrupted else "") + "\n")

def main(prg):

    print "called..."

    port = 5123
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("", port))
    s.listen(1)
    conn, addr = s.accept()

    print "connected..."

    def fin(ret):
        finished(conn, ret, False)

    def model(model):
        on_model(conn, model)

    try:
        recv  = Receiver(conn)
        state = States.IDLE
        k     = 0
        tick = 0

        prg.ground([("base",[]), ("volatile", [tick])])
        prg.assign_external(clingo.Function("now", [tick]), True)
        while True:
            if state == States.SOLVE:
                f = prg.solve_async(
                    model,
                    fin
                )

            msg = recv.readline()
            if state == States.SOLVE:
                f.cancel()
                ret = f.get()
            else:
                ret = SolveResult.unknown
            if msg == "interrupt":
                state = States.IDLE
            elif msg == "exit":
                return
            elif msg.startswith("tick "):
                prg.release_external(clingo.Function("now", [tick]))

                prg.cleanup()
                tick = int(msg.split(" ").pop())
                print "Current Tick: "+ str(tick)

                parts = []
                parts.append(("volatile", [tick]))
                parts.append(("signals", [tick]))
                print parts
                prg.ground(parts)

                prg.assign_external(clingo.Function("now",[tick]), True)
                state = States.IDLE
            elif msg.startswith("signal "):
                tokens = msg.split(" ")
                inputSignals = tokens[1:]
                print "Signals: " + str(inputSignals)
                for s in inputSignals:
                    atom = clingo.Function(str(s),[tick])
                    print "Atom ", atom
                    prg.assign_external(atom, True)
                state = States.IDLE
            elif msg == "solve":
                state = States.SOLVE
            else: raise(RuntimeError("unexpected message: " + msg))
    finally:
        conn.close()
#end.

