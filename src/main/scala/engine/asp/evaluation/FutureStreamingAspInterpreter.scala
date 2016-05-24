package engine.asp.evaluation

import core.Atom
import core.lars.TimePoint
import engine.{Result, Stream}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class FutureStreamingAspInterpreter(private val aspEvaluation: AspEvaluation, waitingAtMost: Duration) extends AspEvaluation {

  def apply(time: TimePoint, dataStream: Stream): Result = {
    val future = Future {
      aspEvaluation(time, dataStream)
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

