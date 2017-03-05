package engine.asp.oneshot

import core.Atom
import core.lars.TimePoint
import engine._

/**
  * Created by FM on 21.04.16.
  */

case class AspPushEvaluationEngine(val aspEvaluation: OneShotEvaluation) extends EvaluationEngine {

  val signalTracker = SignalTracker(aspEvaluation.program)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  def prepare(time: TimePoint) = {
    val result = aspEvaluation(time, signalTracker.tupleCount, signalTracker.allTimePoints(time).toSet)

    cachedResults.put(time, result)
    result
  }

  def evaluate(time: TimePoint) = {
    signalTracker.discardOutdatedSignals(time)

    cachedResults.getOrElse(time, prepare(time))
  }

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    signalTracker.trackSignals(time, atoms)

    val keysToRemove = cachedResults.keySet filter (_.value >= time.value)
    keysToRemove foreach cachedResults.remove

    // TODO: implement invalidation of result
    // a results.remove(time) is probably not enough
    prepare(time)
  }
}

