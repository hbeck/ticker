package engine.asp.oneshot

import clingo.ClingoProgramWithLars
import core._
import core.asp.AspFact
import core.lars.{LarsBasedProgram, LarsProgram, TimePoint}
import engine.asp._
import engine.asp.tms.Pin
import engine.{Result, _}

/**
  * Created by FM on 13.05.16.
  */
trait OneShotEvaluation {
  val program: ClingoProgramWithLars

  def apply(timePoint: TimePoint, dataStream: SignalStream): Result
}

case class OneShotEvaluationEngine(program: ClingoProgramWithLars, interpreter: StreamingAspInterpreter) extends OneShotEvaluation {

  val convertToPinned = PinnedModelToLarsModel(program)

  def apply(time: TimePoint, dataStream: SignalStream): Result = {

    val nowFact = AspFact(now(time))
    val cntFact = AspFact(cnt(IntValue(dataStream.map(_.position).max.toInt)))


    val signals = dataStream flatMap { s =>
      Seq(
        AspFact[AtomWithArgument](PinnedAtom(s.atom, s.time)),
        AspFact[AtomWithArgument](PinnedAtom.asCount(s.atom, Value(s.position.toInt)))
      )
    }


    val aspResult = interpreter(time, signals ++ Seq(nowFact, cntFact))

    val result = aspResult match {
      case Some(model) => Some(convertToPinned(time, model))
      case None => None
    }

    new Result {
      override def get: Option[Model] = result
    }
  }
}

object OneShotEvaluationEngine {

  def pinnedInput(time: TimePoint, dataStream: Stream) = pin(dataStream) + Pin(time)(now)

  def pin(dataStream: Stream): PinnedStream = dataStream flatMap (x => x.atoms map Pin(x.time).apply)

}