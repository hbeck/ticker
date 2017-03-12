package clingo.reactive

import java.io.PrintStream

import clingo.{ClingoAtom, ClingoModel}
import common.Resource
import core.Value

import scala.io.BufferedSource
import scala.tools.nsc.io.Socket


/**
  * Created by fm on 25/01/2017.
  */

case class ClingoTick(parameter: TickParameter, value: Long)

case class ReactiveClingoSignal(predicate: ClingoAtom, arguments: Seq[Value], ticks: Seq[ClingoTick])

object ReactiveClingoClient {
  def connect(port: Int = 5123): ReactiveClingoClient = {
    val socket = Socket.localhost(port)
    socket.either match {
      case Left(ex) => throw new RuntimeException("Could not connect to clingo", ex)
      case Right(s) => {
        new ReactiveClingoClient(s)
      }
    }
  }
}

class ReactiveClingoClient(socket: Socket) extends Resource {
  private val in = new BufferedSource(socket.inputStream())
  private val lines = in.getLines()
  private val out = new PrintStream(socket.outputStream())

  private def terminateConnection() = {
    out.flush()
    out.close()

    // give clingo the possibility to send answers
    Thread.sleep(100)

    in.close()

    socket.close()
  }

  private def send(command: String, arguments: Seq[String] = Seq()) = {
    if (arguments.nonEmpty)
      sendCommand(command + " " + arguments.mkString(" "))
  }

  private def sendCommand(command: String) = {
    out.println(command)
  }

  private def fetchResult() = {
    if (lines.hasNext)
      Some(lines.next())
    else
      None
  }

  def close() = {
    sendCommand("exit")

    // Clingo might send an answer - make sure everything is processed
    Thread.sleep(100)

    this.terminateConnection()
  }

  def sendSignal(signals: Seq[ReactiveClingoSignal]): Unit = {
    val signalEncoding: Seq[String] = convertToSignalArguments(signals)

    send("signal", signalEncoding)
  }

  def sendExpire(signals: Seq[ReactiveClingoSignal]): Unit = {
    val signalEncoding: Seq[String] = convertToSignalArguments(signals)

    send("expire", signalEncoding)
  }

  private def convertToSignalArguments(signals: Seq[ReactiveClingoSignal]) = {
    def concatParts(s: ReactiveClingoSignal) = Seq(s.predicate) ++ s.ticks.map(_.value.toString) ++ s.arguments

    val signalEncoding = signals.
      map(concatParts).
      map(_.mkString(":"))

    signalEncoding
  }


  def evaluate(ticks: Seq[ClingoTick]): Option[Set[ClingoModel]] = {
    val ticksAsString = ticks.map(t => t.parameter + ":" + t.value)
    send("solve", ticksAsString)

    val model = fetchResult()
    model match {
      case Some(m) if m.startsWith("Answer:") => {
        val finished = fetchResult()

        parseResult(m, finished.get)
      }
      case _ => None
    }
  }

  def parseResult(model: String, finished: String): Option[Set[ClingoModel]] = {

    if (finished.endsWith("SAT")) {
      val lines = model.replace("Answer:", "")
        .linesWithSeparators
        .map(_.stripMargin)
        // clingo outputs warnings for undefined atoms
        .filterNot(line => line.startsWith("-:") || line.trim.isEmpty)
        .map(_.stripLineEnd)
        .toList


      val models = lines.
        map(line => line.split(' ').map(_.stripMargin).filterNot(_.isEmpty).toSet)

      // An (satisfiable) but empty model is not a result for us
      if (models.nonEmpty)
        return Some(models.toSet)
    }

    None
  }
}
