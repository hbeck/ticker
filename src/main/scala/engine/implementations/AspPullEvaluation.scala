package engine.implementations

import asp.{Asp, AspConversion}
import core.{AtomWithArguments, Fact, Program}
import engine._

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by FM on 21.04.16.
  */

case class AspPullEvaluation(private val initialProgram: Program) extends EvaluationEngine {

  val convertedProgram = AspConversion(initialProgram)
  val aspEngine = Asp()

  val atomStream: OrderedAtomStream = new OrderedAtomStream

  val cachedResults = scala.collection.mutable.HashMap[Time, Result]()

  def prepare(time: Time) = {
    val facts = StreamingAspTransformation.transform(time, atomStream.evaluate(time))
    val future = Future {
      val result = aspEngine(convertedProgram ++ facts)

      // TODO: should we remove time(now)?
      // TODO: how do we differentiate if the model is defined or not? Adding the fact now(T) leads to a model
      result.headOption match {
        case Some(model) => {
          val atoms = model.filterNot {
            case AtomWithArguments(baseAtom, _) => baseAtom == StreamingAspTransformation.now
            case _ => false
          }
          Some(atoms)
        }
        case None => None
      }
    }
    cachedResults.put(time, FutureResult(future))
  }

  def evaluate(time: Time) = {
    if (!cachedResults.contains(time))
      prepare(time)
    cachedResults(time)
  }

  override def append(time: Time)(atoms: Atom*): Unit = {
    atomStream.append(time)(atoms.toSet)
    // TODO: implement invalidation of result
    // the remove is probably not enough (==> invalidate previous fetched results)
    cachedResults.remove(time)
  }
}
