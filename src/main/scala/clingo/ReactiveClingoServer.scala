package clingo

import java.io.PrintStream
import java.net.{ServerSocket, Socket}
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.BufferedSource

/**
  * Created by fm on 25/01/2017.
  */
class ReactiveClingoServer(port: Int = 5123) {
  private val server = new ServerSocket(port)

  private var connected: Option[ConnectedClingo] = None

  def clingo = connected match {
    case Some(c) => c
    case None => throw new RuntimeException("Connect to Clingo first!")
  }

  def connect() = {
    Future {
      val connectedClingo = new ConnectedClingo(server.accept())

      connected = Some(connectedClingo)

      connectedClingo.communicateOverSocket()
    }
  }

  def sendTick(tick: Long) = {
    clingo.sendCommand("tick " + tick)
    //
    //    val result = connected.result()
    //    assert(result.contains(tick.toString))
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
    var running = true
    val in = new BufferedSource(socket.getInputStream()).getLines()
    val out = new PrintStream(socket.getOutputStream())

    val commands: mutable.Queue[String] = mutable.Queue()
    val results: BlockingQueue[String] = new LinkedBlockingQueue[String]()

    def terminate = {
      running = false
      socket.close()
    }

    def sendCommand(command: String) = {
      out.println(command)
    }

    def result() = results.take()

    def communicateOverSocket() = {
      while (running) {
        val result = in.next()
        results.put(result)

        //        val command = commands.dequeue()


      }
    }
  }

}
