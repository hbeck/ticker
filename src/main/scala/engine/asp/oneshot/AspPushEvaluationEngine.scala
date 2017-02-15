package engine.asp.oneshot

import core.Atom
import core.lars.TimePoint
import engine._

/**
  * Created by FM on 21.04.16.
  */

case class AspPushEvaluationEngine(val aspEvaluation: OneShotEvaluation) extends EvaluationEngine {

  val atomStream = AtomTracking(aspEvaluation.program)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  def prepare(time: TimePoint) = {
    // TODO: decide if we want to use evaluateUntil or the whole stream
    val result = aspEvaluation(time, atomStream.allTimePoints(time).toSet)

    cachedResults.put(time, result)
    result
  }

  def evaluate(time: TimePoint) = {
    atomStream.discardOutdatedAtoms(time)

    cachedResults.getOrElse(time, prepare(time))
  }

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    atomStream.trackAtoms(time, atoms)

    val keysToRemove = cachedResults.keySet filter (_.value >= time.value)
    keysToRemove foreach cachedResults.remove

    // TODO: implement invalidation of result
    // a results.remove(time) is probably not enough
    prepare(time)
  }
}

