package engine.implementations

import asp.Asp
import core.{Fact, Program}
import engine._

/**
  * Created by FM on 21.04.16.
  */

case class AspPushEvaluation(private val initialProgram: Program) extends EvaluationEngine {

  val aspEngine = Asp()

  // TODO: we don' really want to handle the stream here
  // BUT: how to we keep track of the atoms then? We would need to change the API completely
  // OR: Pass the current stream in during the evaluation
  // ---> no real state maintenance in the engine possible (because it might be different every call)

  // how do we outdate information in the intensional stream (see EngineSpec tests)?
  // all data is passed into the ASP-Engine as Facts
  // -> therefor programs like  (b :- not c.) with input (b.  c.) will never invalidate b. (should it even?)

  // are programs required in a different 'language' to model that?

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
