package engine.connectors

import core.lars.TimePoint
import reasoner.Result
import engine.{ConnectToEngine, Engine, Startable}

/**
  * Created by FM on 14.11.16.
  */
object OutputToStdOut extends ConnectToEngine {

    def startWith(engine: Engine): Startable = {
      engine.registerOutput(evaluateModel(engine))

      () => {
        /* NOOP */
      }
    }

    def evaluateModel(engineRunner: Engine)(result: Result, ticks: TimePoint): Unit = {

      val timeInOutput = engineRunner.convertToClockTime(ticks).toSeconds
      result.get match {
        case Some(m) => println(f"Model at T $timeInOutput: $m")
        case None => println(f"No model at T $timeInOutput")
      }
  }
}