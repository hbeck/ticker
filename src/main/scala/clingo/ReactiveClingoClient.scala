package clingo

import java.io.PrintStream
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import core.{Predicate, Value}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.BufferedSource
import scala.tools.nsc.io.Socket

case class Tick(parameter: TickParameter, value: Long)

/**
  * Created by fm on 25/01/2017.
  */

case class ReactiveClingoSignal(predicate: ClingoAtom, arguments: Seq[Value], ticks: Seq[Tick])

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

class ReactiveClingoClient(socket: Socket) {
  private val in = new BufferedSource(socket.inputStream())
  private val lines = in.getLines()
  private val out = new PrintStream(socket.outputStream())

  private def terminateConnection() = {
    out.flush()
    out.close()

    in.close()

    socket.close()
  }

  private def sendCommand(command: String) = {
    out.println(command)
  }

  private def fetchResult() = lines.next()

  def terminate() = {
    sendCommand("exit")

    // Clingo might send an answer - make sure everything is processed accordingly
    Thread.sleep(100)

    this.terminateConnection()
  }

  def sendSignal(signals: Seq[ReactiveClingoSignal]): Unit = {
    def concatParts(s: ReactiveClingoSignal) = Seq(s.predicate) ++ s.ticks.map(_.value.toString) ++ s.arguments

    val signalEncoding = signals.
      map(concatParts).
      map(_.mkString(":"))

    sendCommand("signal " + signalEncoding.mkString(" "))
  }

  def evaluate(ticks: Seq[Tick]): Option[Set[ClingoModel]] = {
    val ticksAsString = ticks.map(t => t.parameter + ":" + t.value)
    sendCommand("solve " + ticksAsString.mkString(" "))

    val model = fetchResult()
    if (model.startsWith("Answer:")) {
      val finished = fetchResult()

      return parseResult(model, finished)
    }

    None
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
