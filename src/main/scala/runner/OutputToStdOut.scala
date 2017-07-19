package runner

import java.util.TimerTask

import core.lars.TimePoint
import engine.{NoResult, Result}

import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.11.16.
  */
case class OutputToStdOut(outputType: OutputEvery) extends ConnectToEngine {

  def startWith(engineRunner: EngineRunner): Startable = {
    engineRunner.registerOutput(evaluateModel(engineRunner))

    () => {
      /* NOOP */
    }
  }

  def evaluateModel(engineRunner: EngineRunner)(result: Result, ticks: TimePoint): Unit = {

    val timeInOutput = engineRunner.convertToInputSpeed(ticks).toSeconds
    result.get match {
      case Some(m) => println(f"Model at T $timeInOutput: $m")
      case None => println(f"No model at T $timeInOutput")
    }
  }
}
