package clingo

import java.io.PrintStream
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.BufferedSource
import scala.tools.nsc.io.Socket

case class TickValue(parameter: TickAtom, value: Long)

/**
  * Created by fm on 25/01/2017.
  */
class ReactiveClingoClient(port: Int = 5123) {
  private val socket = Socket.localhost(port)

  private var connected: Option[ConnectedClingo] = None

  def clingo = connected match {
    case Some(c) => c
    case None => throw new RuntimeException("Connect to Clingo first!")
  }

  def connect() = {

    socket.either match {
      case Left(ex) => throw new RuntimeException("Could not connect to clingo", ex)
      case Right(s) => {

        val connectedClingo = new ConnectedClingo(s)

        connected = Some(connectedClingo)

        //        connectedClingo.communicateOverSocket()
      }
    }
  }

  def terminate() = {
    clingo.sendCommand("exit")

    clingo.terminate
  }

  def sendTick(ticks: Seq[TickValue]) = {
    val ticksAsString = ticks.map(t => t.parameter + ":" + t.value)
    clingo.sendCommand("tick " + ticksAsString.mkString(" "))
  }

  def sendSignal(signals: Seq[ClingoAtom]) = clingo.sendCommand("signal " + signals.mkString(" "))

  def evaluate(): Option[Set[ClingoModel]] = {

    clingo.sendCommand("solve")

    val model = clingo.result()
    if (model.startsWith("Answer:")) {
      val finished = clingo.result()

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

  class ConnectedClingo(socket: Socket) {

    val in = new BufferedSource(socket.inputStream()).getLines()
    val out = new PrintStream(socket.outputStream())

    def terminate = {
      socket.close()
    }

    def sendCommand(command: String) = {
      out.println(command)
    }

    def result() = in.next()

  }

}
