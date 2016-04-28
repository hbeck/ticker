package engine.implementations

import engine.{Atom, Evaluation, Result, Time}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by FM on 24.04.16.
  */
case class FutureAspEvaluation(private val aspEvaluation: AspEvaluation, waitingAtMost: Duration) extends AspEvaluation {

  override def prepare(time: Time, evaluations: Set[Evaluation]): Result = {
    val future = Future {
      aspEvaluation.prepare(time, evaluations)
    }

    FutureResult(future, waitingAtMost)
  }
}

case class FutureResult(future: Future[Result], waitingAtMost: Duration = 1 second) extends Result {
  // look at http://alvinalexander.com/scala/concurrency-with-scala-futures-tutorials-examples
  // prefer non blocking
  // additional option:
  // use a producer/consumer scenario
  // append adds to queue
  // we have a separate thread feeding from queue by evaluating it
  // see https://twitter.github.io/scala_school/concurrency.html
  override def get: Option[Set[Atom]] = Await.result(future, waitingAtMost).get
}

