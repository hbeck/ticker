package engine.connectors

import java.io.PrintStream
import java.net.{ServerSocket, Socket}

import com.typesafe.scalalogging.Logger
import common.{Resource, Util}
import core.lars.TimePoint
import engine.{ConnectToEngine, Engine, Startable}
import reasoner.Result

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
        if (Util.log_level == Util.LOG_LEVEL_DEBUG) {
          logger.debug("New socket connection received")
        }
        new PrintServerSocket(socket).start()
      }
    }
  }

  def evaluateModel(engine: Engine)(result: Result, timepoint: TimePoint): Unit = {
    result.get match {
      case Some(model) => outputToAllClients(Messages.model(engine,timepoint,model))
      case None => outputToAllClients(Messages.noModel(engine,timepoint))
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
