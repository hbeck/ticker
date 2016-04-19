package engine

import asp.Asp
import core.{Atom, Evaluation, Fact, Program}
import jtms.AnswerUpdateNetwork

import scala.collection.{SortedMap, mutable}

/**
  * Created by FM on 05.04.16.
  */
case class AnswerUpdateNetworkEngine(private val program: Program) extends EvaluationEngine {
  val intensionalAtomStream: OrderedAtomStream = new OrderedAtomStream

  val answerUpdateNetwork = AnswerUpdateNetwork(program)

  def append(time: Time)(atoms: Atom*): Unit = {
    intensionalAtomStream.append(time)(atoms.toSet)
  }

  def evaluate(time: Time): Set[Atom] = {
    val facts = intensionalAtomStream.evaluate(time).map(x => Fact(Atom(x.name)))
    facts foreach answerUpdateNetwork.add

    return answerUpdateNetwork.getModel() match {
      case Some(atoms) => atoms.map(x => EngineAtom(x.toString))
      case None => Set()
    }
  }
}

case class AspEvaluation(private val initialProgram: Program) extends EvaluationEngine {

  val aspEngine = Asp()

  val intensionalAtomStream: OrderedAtomStream = new OrderedAtomStream

  def evaluate(time: Time): Set[Atom] = {
    val facts = intensionalAtomStream.evaluate(time) map (x => Fact(Atom(x.name)))
    val result = aspEngine(initialProgram ++ facts.toList)
    result.headOption.getOrElse(Set()).map(x => EngineAtom(x.toString))
  }

  override def append(time: Time)(atoms: Atom*): Unit = {
    intensionalAtomStream.append(time)(atoms.toSet)
  }
}