package engine.connectors

import java.util.concurrent.TimeUnit

import com.typesafe.scalalogging.Logger
import common.Util
import engine._

/**
  * Created by FM on 14.11.16.
  */
case class ReadFromStdIn(inputUnit: TimeUnit) extends ConnectToEngine {

  val logger = Logger("ReadFromStdIn")

  def startWith(runner: Engine): Startable = {

    if (Util.log_level >= Util.LOG_LEVEL_INFO) {
      logger.info("Receiving input from keyboard: <atom>;<atom>;...")
    }

    () => {
      Iterator.continually(scala.io.StdIn.readLine).
        filter(_ != null).
        map(parseInput(_)).
        takeWhile(_.nonEmpty).
        foreach(input => runner.append(None, input))
    }

//    () => {
//      Iterator.continually(scala.io.StdIn.readLine).
//        filter(_ != null).
//        map(parseInputDepr(inputUnit)).
//        takeWhile(_._2.nonEmpty).
//        foreach(input => runner.append(input._1.map(runner.convertToTimePoint), input._2))
//    }
  }
}
