package engine.implementations

import asp.Asp
import core.{Fact, Program}
import engine._

/**
  * Created by FM on 21.04.16.
  */

case class AspPushEvaluation(private val initialProgram: Program) extends EvaluationEngine {

  val aspEngine = Asp()

  val atomStream: OrderedAtomStream = new OrderedAtomStream

  val cachedResults = scala.collection.mutable.HashMap[Time, Result]()

  def prepare(time: Time) = {
    val facts = atomStream.evaluate(time) map (x => Fact(x))
    // TODO add 'now'fact
    val result = aspEngine(initialProgram ++ facts.toList)

    cachedResults.put(time, new Result {
      override def value(): Option[Set[Atom]] = result.headOption match {
        case Some(model) => Some(model.toSet)
        case None => None
      }
    })
  }

  def evaluate(time: Time) = {
    cachedResults(time)
  }

  override def append(time: Time)(atoms: Atom*): Unit = {
    atomStream.append(time)(atoms.toSet)
    // TODO: implement invalidation of result
    // a results.remove(time) is probably not enough
    prepare(time)
  }
}
