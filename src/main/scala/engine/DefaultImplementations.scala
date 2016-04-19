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

  // TODO: we don' really want to handle the stream here
  // BUT: how to we keep track of the atoms then? We would need to change the API completely
  // OR: Pass the current stream in during the evaluation
  // ---> no real state maintenance in the engine possible (because it might be different every call)

  // how do we outdate information in the intensional stream (see EngineSpec tests)?
  // all data is passed into the ASP-Engine as Facts
  // -> therefor programs like  (b :- not c.) with input (b.  c.) will never invalidate b. (should it even?)

  // are programs required in a different 'language' to model that?


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