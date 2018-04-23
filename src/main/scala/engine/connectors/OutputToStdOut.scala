package engine.connectors

import core.lars.TimePoint
import engine.{ConnectToEngine, Engine, Startable}
import reasoner.Result

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

  def evaluateModel(engine: Engine)(result: Result, timepoint: TimePoint): Unit = {
    result.get match {
      case Some(model) => println(Messages.model(engine,timepoint,model))
      case None => println(Messages.noModel(engine,timepoint))
    }
  }

}
