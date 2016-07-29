package engine.incremental

import core.Atom
import core.asp.{AspFact, NormalProgram}
import core.lars.TimePoint
import engine.{Result, _}
import jtms.JtmsGreedy

/**
  * Created by FM on 05.04.16.
  *
  * TODO deprecated
  */
case class AnswerUpdateEvaluation(private val program: NormalProgram) extends EvaluationEngine {
  val extensionalAtomStream: OrderedAtomStream = new OrderedAtomStream

  val answerUpdateNetwork = JtmsGreedy(program)

  def append(time: TimePoint)(atoms: Atom*): Unit = {
    extensionalAtomStream.append(time)(atoms.toSet)
  }

  def evaluate(time: TimePoint) = {
    val facts = extensionalAtomStream.evaluate(time).map(x => AspFact(x))
    facts foreach answerUpdateNetwork.add

    new Result {
      override def get(): Option[Set[Atom]] = {
        answerUpdateNetwork.getModel()
      }
    }
  }
}

