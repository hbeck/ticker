#script (python)

import socket
import sys
import os
import errno
import clingo

from collections import OrderedDict
from clingo import SolveResult, Model


class Receiver:
    def __init__(self, conn):
        self.conn = conn
        self.data = bytearray()

    def readline(self):
        pos = self.data.find("\n")
        while pos < 0:
            while True:
                try:
                    self.data.extend(self.conn.recv(4096))
                except socket.error as (code, msg):
                    if code != errno.EINTR: raise
                else:
                    break
            pos = self.data.find("\n")
        msg = self.data[:pos]
        self.data = self.data[pos + 1:]
        return msg


class States:
    SOLVE = 1
    IDLE = 2


def printModel(model):
    # print "Model ", dir(model)
    print "Atoms ", model.symbols(True, True)


def on_model(conn, model):
    printModel(model)
    conn.sendall("Answer: " + str(model) + "\n")


def finished(conn, ret, interrupted):
    conn.sendall("finish:" + str(ret) + (":INTERRUPTED" if interrupted else "") + "\n")

def asNumber(value):
    if(isinstance(value, (int, long))):
        return value
    if(value.isdigit()):
        return int(value)
    else:
        return value


def atomFromTick(atom, value):
    return clingo.Function(str(atom), [asNumber(value)])


def atomFromItem(item):
    return atomFromTick(item[0], item[1])


def atomsFromTicks(ticks):
    return map(atomFromItem, ticks.items())

def parseTicks(msg):
    tickComponents = str(msg).split(" ")[1:]  # first one is "tick"

    def splitParts(part):
        parts = part.split(":")
        return [
            parts[0], # the atom name
            asNumber(parts[1]) # count/number of atom
        ]

    tickParts = map(splitParts, tickComponents)

    return OrderedDict(tickParts)


def parameters(atomAsString):
    paramBegin = atomAsString.find('(')
    # no parameters
    if(paramBegin < 0):
        return []

    stringParams = atomAsString[paramBegin + 1:].replace(')','')
    print stringParams

    params = stringParams.split(',')

    return map(asNumber, params)

def main(prg):
    print "called..."

    port = 5123
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    for i in range(0, 10):
        try:
            s.bind(("", port))
        except socket.error as (code, msg):
            print msg
            if code != errno.EADDRINUSE: raise

            continue
        else:
            print "waiting for connections..."
            break
        raise "no port found"

    s.listen(1)
    conn, addr = s.accept()

    print "connected..."

    def fin(ret):
        finished(conn, ret, False)

    def model(model):
        on_model(conn, model)

    try:
        recv = Receiver(conn)
        state = States.IDLE
        k = 0
        ticks = OrderedDict([('now', 0), ('cnt', 0)])

        prg.ground([("base", []), ("volatile", ticks.values())])

        for atom in atomsFromTicks(ticks):
            prg.assign_external(atom, True)

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
                conn.close()
                s.close()
                return
            elif msg.startswith("tick "):
                # nicht mehr notwendig (sondern beim expire von signals)
                for atom in atomsFromTicks(ticks):
                    prg.release_external(atom)

                prg.cleanup()
                ##

                ticks = parseTicks(msg)

                print ticks

                parts = []
                parts.append(("volatile", ticks.values()))
                parts.append(("signals", ticks.values()))

                print parts
                prg.ground(parts)

                # nicht mehr notwendig (sondern beim expire von signals)
                for atom in atomsFromTicks(ticks):
                    prg.assign_external(atom, True)

                ###

                state = States.IDLE
            elif msg.startswith("signal "):
                tokens = str(msg).split(" ")
                inputSignals = tokens[1:]
                print "Signals: " + str(inputSignals)
                for s in inputSignals:
                    predicate = s.split('(')[0]
                    params = parameters(s)

                    atom = clingo.Function(predicate, params)
                    print "Atom ", atom
                    prg.assign_external(atom, True)
                state = States.IDLE
            elif msg == "solve":
                state = States.SOLVE
            else:
                raise (RuntimeError("unexpected message: " + msg))
    finally:
        # conn.close()
        s.close()

#end.
