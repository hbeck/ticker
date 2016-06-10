package engine.incremental

import core.Atom
import core.asp.{AspFact, NormalProgram}
import core.lars.TimePoint
import engine.{Result, _}
import jtms.JtmsExtended

/**
  * Created by FM on 05.04.16.
  */
case class IncrementalEvaluation(private val program: NormalProgram) extends EvaluationEngine {
  val intensionalAtomStream: OrderedAtomStream = new OrderedAtomStream

  val answerUpdateNetwork = JtmsExtended(program)

  def append(time: TimePoint)(atoms: Atom*): Unit = {
    intensionalAtomStream.append(time)(atoms.toSet)
  }

  def evaluate(time: TimePoint) = {
    val facts = intensionalAtomStream.evaluate(time).map(x => AspFact(x))
    facts foreach answerUpdateNetwork.add

    new Result {
      override def get(): Option[Set[Atom]] = {
        answerUpdateNetwork.getModel()
      }
    }
  }
}

