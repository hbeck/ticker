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

  def evaluate(time: Time) = {
    val facts = intensionalAtomStream.evaluate(time).map(x => Fact(x))
    facts foreach answerUpdateNetwork.add

    new Result {
      override def value(): Option[Set[Atom]] = {
        answerUpdateNetwork.getModel()
      }
    }
  }
}

