package runner.connectors

import java.io.PrintStream
import java.net.{ServerSocket, Socket}

import com.typesafe.scalalogging.Logger
import common.Resource
import core.lars.TimePoint
import reasoner.Result
import jtms.in
import runner.{ConnectToEngine, Engine, Startable}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.io.BufferedSource

case class OutputToSocket(port: Int) extends ConnectToEngine with Resource {

  val logger = Logger[OutputToSocket]

  type ConnectedClients = (Socket, PrintStream)

  var clients: Seq[ConnectedClients] = Seq()

  def startWith(engineRunner: Engine): Startable = {
    engineRunner.registerOutput(evaluateModel(engineRunner))
    val server = new ServerSocket(port)
    server.setReuseAddress(true)

    () => {
      while (true) {
        val socket = server.accept()
        logger.debug("New socket connection received")
        new PrintServerSocket(socket).start()
      }
    }
  }

  def evaluateModel(engineRunner: Engine)(result: Result, ticks: TimePoint): Unit = {

    val timeInOutput = engineRunner.convertToInputSpeed(ticks).toSeconds
    result.get match {
      case Some(m) => outputToAllClients(f"Model at T $timeInOutput: $m")
      case None => outputToAllClients(f"No model at T $timeInOutput")
    }
  }

  private def outputToAllClients(message: String) = clients.foreach(o => {
    o._2.println(message)
    o._2.flush()
  })

  override def close(): Unit = clients.foreach(o => {
    o._2.close()
    o._1.close()
  })

  private class PrintServerSocket(socket: Socket) extends Thread("Print Model To Socket") {
    override def run(): Unit = {
      val out = new PrintStream(socket.getOutputStream())

      clients = clients :+ (socket, out)
    }
  }

}
