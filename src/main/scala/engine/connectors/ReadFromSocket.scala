package engine.connectors

import java.net.{InetAddress, Socket}

import com.typesafe.scalalogging.Logger
import common.Util
import core.lars.TimeUnit
import engine.{ConnectToEngine, Engine, Startable}

import scala.io.BufferedSource
import scala.util.Try

/**
  * Created by FM on 14.11.16.
  */
case class ReadFromSocket(inputUnit: TimeUnit, port: Int) extends ConnectToEngine {

  val logger = Logger[ReadFromSocket]

  //private val parser = parseInputDepr(inputUnit) _

  def startWith(engineRunner: Engine): Startable = {
    () => {
      val init = Try(connectToSocket(engineRunner))
      if (init.isFailure) {
        logger.warn("InputSocket connection could not be initialized. Continuing without an input-socket!", init.failed.get)
      }
    }
  }

  private def connectToSocket(engineRunner: Engine) = {

    val socket = new Socket(InetAddress.getByName("localhost"), port)

    lazy val in = new BufferedSource(socket.getInputStream).getLines()

    in.foreach(input => {
      val atoms = parseInput(input)
      if (atoms.nonEmpty) {
        engineRunner.append(None, atoms)
        if (Util.log_level == Util.LOG_LEVEL_DEBUG) {
          logger.debug(f"appending atoms from socket: $atoms")
        }
      } else {
        if (Util.log_level == Util.LOG_LEVEL_DEBUG) {
          logger.debug(f"could not parse input $input")
        }
      }
    })

//    in.foreach(input => {
//      val (time, atoms) = parser(input)
//
//      if (atoms.nonEmpty) {
//        engineRunner.append(time.map(engineRunner.convertToTimePoint), atoms)
//        if (Util.log_level == Util.LOG_LEVEL_DEBUG) {
//          logger.debug(f"OK,  appending @$time $atoms from socket")
//        }
//      }
//      else {
//        if (Util.log_level == Util.LOG_LEVEL_DEBUG) {
//          logger.debug(f"could not parse input $input")
//        }
//      }
//    })
  }
}
