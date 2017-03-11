package engine.asp.oneshot

import clingo.ClingoProgramWithLars
import core._
import core.asp.AspFact
import core.lars.TimePoint
import engine.asp._
import engine.{Result, _}

/**
  * Created by FM on 13.05.16.
  */
trait OneShotEvaluation {
  val program: ClingoProgramWithLars

  def apply(timePoint: TimePoint, count: Long, dataStream: SignalStream): Result
}

case class OneShotEvaluationEngine(program: ClingoProgramWithLars, interpreter: StreamingAspInterpreter) extends OneShotEvaluation {

  def apply(time: TimePoint, count: Long, dataStream: SignalStream): Result = {

    val nowFact = AspFact(now(time)).asInstanceOf[PinnedFact]
    val cntFact= AspFact(cnt(IntValue(count.toInt))).asInstanceOf[PinnedFact]

    val signals = dataStream flatMap { s =>
      Seq[AspFact[AtomWithArgument]](
        AspFact(PinnedAtom.asPinnedAtAtom(s.signal, s.time)),
        //AspFact(PinnedAtom.asPinnedCntAtom(s.signal, Value(s.count.toInt))),
        AspFact(tickPredicate(Seq(s.time,Value(s.count.toInt))).asInstanceOf[AtomWithArgument]),
        AspFact(PinnedAtom.asPinnedAtCntAtom(s.signal, s.time, Value(s.count.toInt)))
      )
    }

    val aspResult = interpreter(time, signals ++ Seq(nowFact, cntFact))

    val result = aspResult match {
      case Some(model) => Some(AspModelToLarsModel(time, model))
      case None => None
    }

    new Result {
      override def get: Option[Model] = result
    }
  }
}
