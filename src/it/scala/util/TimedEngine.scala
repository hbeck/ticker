package util

import core.Atom
import core.lars.TimePoint
import engine.{Engine, Result}

import scala.concurrent.duration.{Deadline, Duration}

/**
  * Created by FM on 06.08.16.
  */
case class TimedEngine(engine: Engine,
                       appendExecutionTimes: collection.mutable.ArrayBuffer[Duration] = collection.mutable.ArrayBuffer(),
                       evaluateExecutionTimes: collection.mutable.ArrayBuffer[Duration] = collection.mutable.ArrayBuffer()) extends Engine {


  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    val start = Deadline.now

    engine.append(time)(atoms: _*)

    val end = Deadline.now

    val elapsed = ((end - start))
    appendExecutionTimes += elapsed
  }

  override def evaluate(time: TimePoint): Result = {
    val start = Deadline.now

    val result = engine.evaluate(time)

    val end = Deadline.now

    val elapsed = ((end - start))
    evaluateExecutionTimes += elapsed

    result
  }
}
