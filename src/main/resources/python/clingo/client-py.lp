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


def tryConnect(port):
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
    return s, conn


class States:
    SOLVE = 1
    IDLE = 2


def printModel(model):
    print "Model ", model
    print "Complement", model.symbols(atoms=True,complement = True)
    print "Atoms ", model.symbols(atoms = True)


def asNumber(value):
    if (isinstance(value, (int, long))):
        return value
    if (value.isdigit()):
        return int(value)
    else:
        return value


def atom(predicate, arguments):
    return clingo.Function(predicate, arguments)


def atomFromTick(predicate, *args):
    return atom(str(predicate), map(asNumber, args))


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

    atomParameters = []
    if len(params) > 2:
        atomParameters = params[2:]

    t = params[0]
    c = params[1]

    return predicate, t, c, atomParameters


def nowAtom(ticks):
    return atomFromTick("now", ticks['t'])


def cntAtom(ticks):
    return atomFromTick("cnt", ticks['c'])


def main(prg):
    print "called..."

    port = 5123
    openSocket, conn = tryConnect(port)

    print "connected..."

    def fin(ret):
        interrupted = False
        conn.sendall("finish:" + str(ret) + (":INTERRUPTED" if interrupted else "") + "\n")

    def model(model):
        printModel(model)
        conn.sendall("Answer: " + str(model) + "\n")

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
                openSocket.close()
                return
            elif msg.startswith("signal "):
                signals = str(msg).split(" ")[1:]  # first element is string "signal"

                for signal in signals:
                    predicate, t, c, atomParameters = signalParameters(signal)
                    print "building signal", predicate, t, c, atomParameters

                    # ground first.
                    functionName = "signals_" + predicate + "_" + str(len(atomParameters))
                    ground(functionName, [t, c] + atomParameters)

                    addExternal(atom("" + predicate + "_ext_at", atomParameters + [t]))
                    addExternal(atom("" + predicate + "_cnt", atomParameters + [c]))
                    addExternal(atom("" + predicate + "_at_cnt", atomParameters + [t, c]))

                    state = States.IDLE
            elif msg.startswith("expire "):
                signals = str(msg).split(" ")[1:]  # first element is string "expire"

                for expire in signals:
                    predicate, t, c, atomParameters = signalParameters(expire)
                    print "expiring signal", predicate, t, c, atomParameters

                    removeExternal(atom("" + predicate + "_ext_at", atomParameters + [t]))
                    removeExternal(atom("" + predicate + "_cnt", atomParameters + [c]))
                    removeExternal(atom("" + predicate + "_at_cnt", atomParameters + [t, c]))

                    state = States.IDLE

            elif msg.startswith("solve "):
                # state = States.SOLVE

                # expire the old now/cnt atoms before we update the tick
                removeExternal(nowAtom(ticks))
                removeExternal(cntAtom(ticks))

                prg.cleanup()

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
        openSocket.close()

# attention: last line must not have space between hash and end!
#end.
