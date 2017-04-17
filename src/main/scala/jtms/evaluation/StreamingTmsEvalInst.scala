package jtms.evaluation

import core.{Atom, Model}
import core.asp.{NormalRule, UserDefinedAspFact, UserDefinedAspRule}

import scala.util.Random

/**
  * Created by hb on 04.04.17.
  */
trait StreamingTmsEvalInst extends LarsEvaluationInstance {

  def random: Random

  val timePoints: Int
  def staticRules(): Seq[NormalRule]
  def factAtomsToAddAt(t: Int): Seq[Atom]
  def factAtomsToRemoveAt(t: Int): Seq[Atom]
  def rulesToAddAt(t: Int): Seq[NormalRule]
  def rulesToRemoveAt(t: Int): Seq[NormalRule]

  def verifyModel(model: Option[Model], t: Int)

  def factsToAddAt(t: Int): Seq[NormalRule] = factAtomsToAddAt(t) map (fact(_))
  def factsToRemoveAt(t: Int): Seq[NormalRule] = factAtomsToRemoveAt(t) map (fact(_))

  //
  //helper methods
  //

  def rule(head: Atom, posBody: Set[Atom], negBody: Set[Atom]): NormalRule = {
    UserDefinedAspRule[Atom](head, posBody, negBody)
  }

  def rule(head: Atom, posBody: Atom): NormalRule = {
    UserDefinedAspRule[Atom](head, Set(posBody), Set())
  }

  def rule(head: Atom, posBody: Atom, negBody: Atom): NormalRule = {
    UserDefinedAspRule[Atom](head, Set(posBody), Set(negBody))
  }

  def fact(head: Atom): NormalRule = UserDefinedAspFact[Atom](head)

}


