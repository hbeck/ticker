package runner

import java.util.TimerTask

import core.lars.TimePoint
import engine.{NoResult, Result}

import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.11.16.
  */
case class OutputToStdOut(onlyModelChanges: Boolean = true) extends ConnectToEngine {
  val timer = new java.util.Timer()

  @volatile var lastModel: Result = NoResult

  def startWith(engineRunner: EngineRunner): Startable = {
    engineRunner.registerOutput(evaluateModel(engineRunner))

    () => {
      /* NOOP */
    }
  }

  def evaluateModel(engineRunner: EngineRunner)(result: Result, ticks: TimePoint): Unit = {

    if (!onlyModelChanges || result.get != lastModel.get) {
      val timeInOutput = engineRunner.convertTicksToOutput(ticks)
      result.get match {
        case Some(m) => println(f"Model at T $timeInOutput: $m")
        case None => println(f"No model at T $timeInOutput")
      }
      lastModel = result
    }
  }
}
