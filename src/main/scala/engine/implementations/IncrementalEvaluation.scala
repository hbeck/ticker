package engine.implementations

import core.{Atom, Fact, AspProgram}
import engine.{Result, _}
import jtms.ExtendedJTMS

/**
  * Created by FM on 05.04.16.
  */
case class IncrementalEvaluation(private val program: AspProgram) extends EvaluationEngine {
  val intensionalAtomStream: OrderedAtomStream = new OrderedAtomStream

  val answerUpdateNetwork = ExtendedJTMS(program)

  def append(time: Time)(atoms: Atom*): Unit = {
    intensionalAtomStream.append(time)(atoms.toSet)
  }

  def evaluate(time: Time) = {
    val facts = intensionalAtomStream.evaluate(time).map(x => Fact(x))
    facts foreach answerUpdateNetwork.add

    new Result {
      override def get(): Option[Set[Atom]] = {
        answerUpdateNetwork.getModel()
      }
    }
  }
}

