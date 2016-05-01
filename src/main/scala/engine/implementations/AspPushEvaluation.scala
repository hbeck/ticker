package engine.implementations

import asp.Asp
import core.{AspFact, AspProgram, Atom}
import engine._

/**
  * Created by FM on 21.04.16.
  */

case class AspPushEvaluation(private val aspEvaluation: AspEvaluation) extends EvaluationEngine {

  val atomStream: OrderedAtomStream = new OrderedAtomStream

  val cachedResults = scala.collection.mutable.HashMap[Time, Result]()

  def prepare(time: Time) = {
    // TODO: decide if we want to use evaluateUntil or the whole stream
    val result = aspEvaluation.prepare(time, atomStream.evaluateUntil(time))

    cachedResults.put(time, result)
  }

  def evaluate(time: Time) = {
    cachedResults.getOrElse(time, EmptyResult)
  }

  override def append(time: Time)(atoms: Atom*): Unit = {
    atomStream.append(time)(atoms.toSet)
    // TODO: implement invalidation of result
    // a results.remove(time) is probably not enough
    prepare(time)
  }
}

object EmptyResult extends Result {
  override def get: Option[Set[Atom]] = None
}