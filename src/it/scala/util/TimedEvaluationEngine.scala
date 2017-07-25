package util

import core.Atom
import core.lars.TimePoint
import engine.{EvaluationEngine, Result}

import scala.concurrent.duration.{Deadline, Duration}

/**
  * Created by FM on 06.08.16.
  */
case class TimedEvaluationEngine(evaluationEngine: EvaluationEngine,
                                 appendExecutionTimes: collection.mutable.ArrayBuffer[Duration] = collection.mutable.ArrayBuffer(),
                                 evaluateExecutionTimes: collection.mutable.ArrayBuffer[Duration] = collection.mutable.ArrayBuffer()) extends EvaluationEngine {


  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    val start = Deadline.now

    evaluationEngine.append(time)(atoms: _*)

    val end = Deadline.now

    val elapsed = ((end - start))
    appendExecutionTimes += elapsed
  }

  override def evaluate(time: TimePoint): Result = {
    val start = Deadline.now

    val result = evaluationEngine.evaluate(time)

    val end = Deadline.now

    val elapsed = ((end - start))
    evaluateExecutionTimes += elapsed

    result
  }
}
