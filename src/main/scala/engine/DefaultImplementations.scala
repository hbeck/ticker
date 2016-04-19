package engine

import asp.Asp
import core.{Atom, Evaluation, Fact, Program}
import jtms.AnswerUpdateNetwork

import scala.collection.{SortedMap, mutable}

/**
  * Created by FM on 05.04.16.
  */
case class AnswerUpdateNetworkEngine(private val program: Program) extends EvaluationEngine {
  val answerUpdateNetwork = AnswerUpdateNetwork(program)

  def append(time: Time)(atoms: Atom*): Unit = {
    // todo: manage time?
    //    atoms map (Fact(_)) foreach answerUpdateNetwork.add
  }

  def evaluate(time: Time): Set[Atom] = answerUpdateNetwork.getModel() match {
    //    case Some(atoms) => atoms
    case None => Set()
  }

}


case class AspEvaluation(private val initialProgram: Program) extends EvaluationEngine  {

  val aspEngine = Asp()

  val intensionalAtomStream: OrderedAtomStream = new OrderedAtomStream

  def evaluate(time: Time): Set[Atom] = {
    val facts = intensionalAtomStream.evaluate(time) map (x => Fact(Atom(x.name)))
    val result = aspEngine(initialProgram ++ facts.toList)
    result.headOption.getOrElse(Set()).map(x => EngineAtom(x.toString))
  }

  override def append(time: Time)(atoms:Atom*): Unit = {
    intensionalAtomStream.append(time)(atoms.toSet)
  }
}

case class AspEvaluation_(private val initialProgram: Program) extends EvaluationEngine {
  val aspEngine = Asp()

  //  val results: scala.collection.mutable.Map[Time, Set[Atom]] = mutable.HashMap()

  var elements = SortedMap.empty[Time, Set[Atom]](Ordering.fromLessThan((l, r) => l.milliseconds < r.milliseconds))

  def append(time: Time)(atoms: Atom*): Unit = {
    elements = elements + ((time, atoms.toSet))

    //    val facts = elements map (x=>Atom(x._2.name)) //(Fact(Atom(_.caption)))
    //    val result = aspEngine(initialProgram ++ facts.toList)
    //    if (result.nonEmpty)
    //      results(time) = result.head
  }

  def evaluate(time: Time): Set[Atom] = {
    //    this.results.getOrElse(time, Set())
    Set()
  }
}