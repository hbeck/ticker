package engine.asp.oneshot

import core.Atom
import core.asp.AspFact
import core.lars.TimePoint
import engine._
import engine.asp.{cnt, now}

/**
  * Created by FM on 21.04.16.
  */

case class AspPullEvaluationEngine(private val aspEvaluation: OneShotEvaluation) extends EvaluationEngine {

  val atomTracker = AtomTracking(aspEvaluation.program)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  def prepare(time: TimePoint) = {

    val result = aspEvaluation(time, atomTracker.allTimePoints(time).toSet)

    cachedResults.put(time, result)
  }

  def evaluate(time: TimePoint) = {
    if (!cachedResults.contains(time))
      prepare(time)

    atomTracker.discardOutdatedAtoms(time)

    cachedResults(time)
  }

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    atomTracker.trackAtoms(time, atoms)
    // TODO: implement invalidation of result
    // the remove is probably not enough (==> invalidate previous fetched results)
    cachedResults.remove(time)
  }
}
