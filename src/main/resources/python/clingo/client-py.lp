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

    parts = map(splitParts, tickComponents)

    return OrderedDict(parts)


def signalParameters(signal):
    parts = signal.split(':')
    predicate = parts[0]
    params = map(asNumber, parts[1:])

    return predicate, params


def nowAtom(ticks):
    return atomFromTick("now", ticks['t'])


def cntAtom(ticks):
    return atomFromTick("cnt", ticks['c'])


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

    def removeExternal(externalAtom):
        print "removing external", externalAtom
        prg.release_external(externalAtom)

    def ground(functionName, params):
        parts = []
        parts.append((functionName, params))
        print "grounding", parts
        prg.ground(parts)

    try:
        recv = Receiver(conn)
        state = States.IDLE
        ticks = OrderedDict([('t', 0), ('c', 0)])

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
            elif msg.startswith("signal "):
                signals = str(msg).split(" ")[1:]  # first element is string "signal"

                for signal in signals:
                    predicate, params = signalParameters(signal)
                    print "building signal", predicate, params

                    atomParameters = []
                    if len(params) > 2:
                        atomParameters = params[2:]

                    t = params[0]
                    c = params[1]

                    # ground first.
                    functionName = "signals_" + predicate + "_" + str(len(atomParameters))
                    ground(functionName, params)

                    addExternal(atom("at_" + predicate, atomParameters + [t]))
                    addExternal(atom("cnt_" + predicate, atomParameters + [c]))

                    state = States.IDLE

            elif msg.startswith("solve "):
                # state = States.SOLVE

                # expire the old now/cnt atoms before we update the tick
                removeExternal(nowAtom(ticks))
                removeExternal(cntAtom(ticks))

                ticks = parseTicks(msg)

                print "Solving with ticks", ticks
                ground("volatile", ticks.values())

                addExternal(nowAtom(ticks))
                addExternal(cntAtom(ticks))

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
