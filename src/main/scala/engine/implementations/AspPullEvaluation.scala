package engine.implementations

import engine._

/**
  * Created by FM on 21.04.16.
  */

case class AspPullEvaluation(private val aspEvaluation: AspEvaluation) extends EvaluationEngine {

  val atomStream: OrderedAtomStream = new OrderedAtomStream

  val cachedResults = scala.collection.mutable.HashMap[Time, Result]()

  def prepare(time: Time) = {
    val result = aspEvaluation.prepare(time, atomStream.evaluateUntil(time))

    cachedResults.put(time, result)
  }

  def evaluate(time: Time) = {
    if (!cachedResults.contains(time))
      prepare(time)

    // TODO implement this correctly
    val smallerKeys = cachedResults.filterKeys(p => p.timePoint <= time.timePoint)
    val aggregatedResult = smallerKeys.flatMap(x => x._2.get.getOrElse(Set()))
    new Result {
      override def get: Option[Set[Atom]] = Some(aggregatedResult.toSet)
    }
    //    cachedResults(time)
  }

  override def append(time: Time)(atoms: Atom*): Unit = {
    atomStream.append(time)(atoms.toSet)
    // TODO: implement invalidation of result
    // the remove is probably not enough (==> invalidate previous fetched results)
    cachedResults.remove(time)
  }
}
