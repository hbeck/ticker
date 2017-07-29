package runner.connectors

import java.io.PrintStream
import java.net.{ServerSocket, Socket}

import common.Resource
import core.lars.TimePoint
import engine.Result
import jtms.in
import runner.{ConnectToEngine, EngineRunner, Startable}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.io.BufferedSource

case class OutputToSocket(port: Int) extends ConnectToEngine with Resource {

  type ConnectedClients = (Socket, PrintStream)

  var clients: Seq[ConnectedClients] = Seq()

  def startWith(engineRunner: EngineRunner): Startable = {
    engineRunner.registerOutput(evaluateModel(engineRunner))

    () => {

      val server = new ServerSocket(9999)

      new Thread(new Runnable {
        override def run(): Unit = while (true) {
          val s = server.accept()

          val out = new PrintStream(s.getOutputStream())

          clients = clients :+ (s, out)
        }
      }).start()
    }
  }

  def evaluateModel(engineRunner: EngineRunner)(result: Result, ticks: TimePoint): Unit = {

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
}
