package engine.implementations

import asp.Asp
import core.{Fact, Program}
import engine._

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Created by FM on 21.04.16.
  */

case class AspPullEvaluation(private val initialProgram: Program) extends EvaluationEngine {

  val aspEngine = Asp()

  val atomStream: OrderedAtomStream = new OrderedAtomStream

  val cachedResults = scala.collection.mutable.HashMap[Time, Result]()

  def prepare(time: Time) = {
    val facts = atomStream.evaluate(time) map (x => Fact(x))
    // TODO add 'now'fact
    val future = Future {
      val result = aspEngine(initialProgram ++ facts.toList)
      result.headOption match {
        case Some(model) => Some(model.toSet)
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

case class FutureResult(future: Future[Option[Set[Atom]]]) extends Result {
  // look at http://alvinalexander.com/scala/concurrency-with-scala-futures-tutorials-examples
  // prefer non blocking
  // additional option:
  // use a producer/consumer scenario
  // append adds tu queue
  // we have a seperat thread feeding from queue by evaluating it
  // see https://twitter.github.io/scala_school/concurrency.html
  override def value: Option[Set[Atom]] = Await.result(future, 1 second)
}