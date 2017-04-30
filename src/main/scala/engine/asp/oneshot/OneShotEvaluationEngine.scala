package engine.asp.oneshot

import clingo.ClingoProgramWithLars
import core._
import core.asp.AspFact
import core.lars.{Box, SlidingTupleWindow, TimePoint, WindowAtom}
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

  val windowAtoms = program.larsRules flatMap (_.body collect { case w:WindowAtom => w})

  val needs_at_cnt_atoms = windowAtoms exists {
    case WindowAtom(SlidingTupleWindow(_), _, _) => true
    case _ => false
  }

  val need_tick_atoms = windowAtoms exists {
    case WindowAtom(SlidingTupleWindow(_), Box, _) => true
    case _ => false
  }

  def apply(time: TimePoint, count: Long, dataStream: SignalStream): Result = {

    val nowFact = AspFact(now(time)).asInstanceOf[PinnedFact]

    val signals = dataStream flatMap { s =>
      val timePinned = Seq[AspFact[AtomWithArguments]](AspFact(PinnedAtom.asPinnedAtAtom(s.signal, s.time)))
      if (needs_at_cnt_atoms) {
        val countPinned = Seq[AspFact[AtomWithArguments]](AspFact(PinnedAtom.asPinnedAtCntAtom(s.signal, s.time, Value(s.count.toInt))))
        if (need_tick_atoms) {
          timePinned ++ countPinned :+ tickFact(s.time,Value(s.count.toInt))
        } else {
          timePinned ++ countPinned
        }
      } else {
        timePinned
      }
    }

    val aspResult = if (needs_at_cnt_atoms) {
      val cntFact= AspFact(cnt(IntValue(count.toInt))).asInstanceOf[PinnedFact]
      interpreter(time, signals ++ Seq(nowFact, cntFact))
    } else {
      interpreter(time, signals ++ Seq(nowFact))
    }

    val result = aspResult match {
      case Some(model) => Some(AspModelToLarsModel(time, model))
      case None => None
    }

    new Result {
      override def get: Option[Model] = result
    }
  }
}
