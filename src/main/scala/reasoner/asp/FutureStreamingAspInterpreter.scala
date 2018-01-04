package reasoner.asp

import core.Atom
import core.lars.TimePoint
import reasoner.asp.clingo.ClingoProgramWithLars
import reasoner.{Result, SignalStream}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class FutureStreamingAspInterpreter(private val clingoEvaluation: ClingoEvaluation, waitingAtMost: Duration) extends ClingoEvaluation {

  def apply(time: TimePoint, count: Long, dataStream: SignalStream): Result = {
    val future = Future {
      clingoEvaluation(time, count, dataStream)
    }

    FutureResult(future, waitingAtMost)
  }

  override val program: ClingoProgramWithLars = clingoEvaluation.program
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

