package engine.asp.oneshot

import clingo.ClingoProgramWithLars
import core.Atom
import core.lars.TimePoint
import engine.{Result, SignalStream, Stream}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class FutureStreamingAspInterpreter(private val aspEvaluation: OneShotEvaluation, waitingAtMost: Duration) extends OneShotEvaluation {

  def apply(time: TimePoint, count: Long, dataStream: SignalStream): Result = {
    val future = Future {
      aspEvaluation(time, count, dataStream)
    }

    FutureResult(future, waitingAtMost)
  }

  override val program: ClingoProgramWithLars = aspEvaluation.program
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

