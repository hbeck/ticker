package engine.implementations

import engine.{Atom, Result, Time}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by FM on 24.04.16.
  */
case class FutureAspEvaluation(private val aspEvaluation: AspEvaluation) extends AspEvaluation {

  override def prepare(time: Time, atoms: Set[Atom]): Result = {
    val future = Future {
      aspEvaluation.prepare(time, atoms)
    }

    FutureResult(future)
  }
}

case class FutureResult(future: Future[Result], waitingAtMost: Duration = 1 second) extends Result {
  // look at http://alvinalexander.com/scala/concurrency-with-scala-futures-tutorials-examples
  // prefer non blocking
  // additional option:
  // use a producer/consumer scenario
  // append adds tu queue
  // we have a seperat thread feeding from queue by evaluating it
  // see https://twitter.github.io/scala_school/concurrency.html
  override def get: Option[Set[Atom]] = Await.result(future, waitingAtMost).get
}

