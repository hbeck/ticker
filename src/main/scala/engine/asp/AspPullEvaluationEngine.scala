package engine.asp

import core.Atom
import core.lars.TimePoint
import engine._
import engine.asp.evaluation.AspEvaluation

/**
  * Created by FM on 21.04.16.
  */

case class AspPullEvaluationEngine(private val aspEvaluation: AspEvaluation) extends EvaluationEngine {

  val atomStream: OrderedAtomStream = new OrderedAtomStream

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  def prepare(time: TimePoint) = {
    val result = aspEvaluation(time, atomStream.evaluateUntil(time))

    cachedResults.put(time, result)
  }

  def evaluate(time: TimePoint) = {
    if (!cachedResults.contains(time))
      prepare(time)

    cachedResults(time)
  }

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    atomStream.append(time)(atoms.toSet)
    // TODO: implement invalidation of result
    // the remove is probably not enough (==> invalidate previous fetched results)
    cachedResults.remove(time)
  }
}
