package jtms.asp.examples

import asp.Asp
import core._
import jtms.evaluation.ExtendedJTMSEvaluation
import org.scalatest.FlatSpec

import scala.language.postfixOps

/**
  * Created by FM on 25.02.16.
  * “*/
trait SingleHusbandBehavior {
  this: FlatSpec =>

  val man = Atom("man")
  val single = Atom("single")
  val husband = Atom("husband")

  val r0 = AspFact(man)
  val r1 = AspRule(single,Set(man),Set(husband)) //single :- man, not husband
  val r2 = AspRule(husband,Set(man),Set(single)) //husband :- man, not single

  val pSingleFirst = AspProgram(r0, r1, r2)
  val pHusbandFirst = AspProgram(r0, r2, r1)

  def singleHusband(evaluation: => Evaluation) {

    /*
        man.
        single :- man, not husband.
        husband :- man, not single.
   */

    it should "include the model man, single" in {
      assert(evaluation(pSingleFirst) contains Set(man, single))
    }

    it should "include the model man, husband" in {
      assert(evaluation(pHusbandFirst) contains Set(man, husband))
    }

  }
}

class SingleHusbandSample extends FlatSpec with SingleHusbandBehavior {
  def net = new ExtendedJTMSEvaluation

  def asp = Asp()

  "The AnserUpdateNetwork implementation " should behave like singleHusband(net)
  "The ASP implementation " should behave like singleHusband(asp)
}