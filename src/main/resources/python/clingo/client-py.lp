#script (python)
# attention: first line must not have space between hash and script!

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
    if (isinstance(value, (int, long))):
        return value
    if (value.isdigit()):
        return int(value)
    else:
        return value

def atom(predicate, arguments):
    return clingo.Function(predicate, arguments)

def atomFromTick(predicate, value):
    return clingo.Function(str(predicate), [asNumber(value)])


def atomFromItem(item):
    return atomFromTick(item[0], item[1])


def atomsFromTicks(ticks):
    return map(atomFromItem, ticks.items())


def parseTicks(msg):
    tickComponents = str(msg).split(" ")[1:]  # first one is "tick"

    def splitParts(part):
        parts = part.split(":")
        return [
            parts[0],  # the atom name
            asNumber(parts[1])  # count/number of atom
        ]

    tickParts = map(splitParts, tickComponents)

    return OrderedDict(tickParts)


def parameters(atomAsString):
    paramBegin = atomAsString.find('(')
    # no parameters
    if (paramBegin < 0):
        return []

    stringParams = atomAsString[paramBegin + 1:].replace(')', '')
    print stringParams

    params = stringParams.split(',')

    return map(asNumber, params)


def signalParameters(params):
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

    def addExternal(externalAtom):
        print "Assign external", externalAtom
        prg.assign_external(externalAtom, True)

    try:
        recv = Receiver(conn)
        state = States.IDLE
        ticks = OrderedDict([('now', 0), ('cnt', 0)])
        #
        # prg.ground([("base", []), ("volatile", ticks.values())])
        #
        # for atomTicks in atomsFromTicks(ticks):
        #     addExternal(atomTicks)

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
                # for atom in atomsFromTicks(ticks):
                #   prg.release_external(atom)

                # prg.cleanup()
                ##

                ticks = parseTicks(msg)

                print ticks

                parts = []
                parts.append(("volatile", ticks.values()))
                parts.append(("signals", ticks.values()))

                print parts
                prg.ground(parts)

                # nicht mehr notwendig (sondern beim expire von signals)
                for tickAtom in atomsFromTicks(ticks):
                    addExternal(tickAtom)

                ###

                state = States.IDLE
            elif msg.startswith("signal "):
                differentSignals = str(msg).split(" ")[1:]  # first element is "signal"

                print "Signals: " + str(differentSignals)
                for signal in differentSignals:
                    signalParts = signal.split(':')
                    predicate = signalParts[0]
                    params = signalParameters(signalParts[1:])

                    # ground first
                    functionName = "signals_" + predicate + "_" + str(len(params) - 2)

                    parts = []
                    parts.append((functionName, params))

                    print "grounding signals", parts
                    prg.ground(parts)

                    atomParameters = []
                    if len(params) > 2:
                        atomParameters = params[2:]

                    t = params[0]
                    c = params[1]

                    addExternal(atom("at_" + predicate, atomParameters + [t]))
                    addExternal(atom("cnt_" + predicate, atomParameters + [c]))

                    state = States.IDLE

            elif msg.startswith("solve "):
                # state = States.SOLVE

                currentTicks = parseTicks(msg)

                print "ticks: ", currentTicks

                parts = []
                parts.append(("volatile", currentTicks.values()))

                print "Grounding/Solving", parts
                prg.ground(parts)

                addExternal(atomFromTick("now", currentTicks['t']))
                addExternal(atomFromTick("cnt", currentTicks['c']))

                # result = prg.solve(model)
                # print result
                # fin(result)
                f = prg.solve_async(
                    model,
                    fin
                )

        else:
            raise (RuntimeError("unexpected message: " + msg))
    finally:
        conn.close()
        s.close()

# attention: last line must not have space between hash and end!
#end.
