package jtms.evaluation

import core.Atom
import core.asp.{NormalRule, UserDefinedAspFact, UserDefinedAspRule}
import jtms.JtmsUpdateAlgorithm

import scala.util.Random

/**
  * Created by hb on 04.04.17.
  */
trait StreamingTmsEvalInstance {

  def random: Random

  def timePoints: Int
  def staticRules(): Seq[NormalRule]
  def factsToAddAt(t: Int): Seq[NormalRule]
  def rulesToAddAt(t: Int): Seq[NormalRule]
  def rulesToRemoveAt(t: Int): Seq[NormalRule]
  def factsToRemoveAt(t: Int): Seq[NormalRule]
  def verifyModel(tms: JtmsUpdateAlgorithm, t: Int)


  //helper methods

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
  def fact(head: String): NormalRule = UserDefinedAspFact[Atom](Atom(head))

}


