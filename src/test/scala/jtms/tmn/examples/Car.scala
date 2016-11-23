package jtms.tmn.examples

import core._
import core.asp.{AspFact, AspProgram, AspRule}
import jtms.asp.examples.EvaluateJtmsImplementations
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
trait CarBehavior {
  this: FlatSpec =>

  val S_not = Atom("car_is_not_starting")
  val G_not = Atom("not_enough_gas")
  val G = Atom("enough_gas")
  val D = Atom("defect")
  val I = Atom("ignition_broken")
  val C = Atom("carb_broken")
  val Falsum = new ContradictionAtom(Predicate("f"))

  val j0 = AspRule.pos(S_not).neg(D).head(G_not);
  val j1 = AspRule.pos(S_not, G).head(D)
  val j2 = AspRule.pos(G, G_not).head(Falsum)
  val j3 = AspRule.pos(I).head(D)
  val j4 = AspRule.pos(C).head(D)

  val notStarting = AspFact(S_not)
  val enoughGas = AspFact(G)
  val notEnoughGas = AspFact(G_not)
  val brokenIgnition = AspFact(C)

  val program = AspProgram(j0, j1, j2, j3, j4)

  def theCar(evaluation: Evaluation): Unit = {
    it should "not result in a defect" in {
      info("When the car is not starting and there is not enough gas")
      val p = program + notStarting + notEnoughGas

      val model = evaluation(p)

      assert(model.contains( Set(D)) == false)
    }

    it should "result in a defect" in {
      info("When the car is not starting and there is enough gas")
      val p = program + notStarting + enoughGas

      val model = evaluation(p)

      assert(model.head.contains(D))
    }

    it should "result in not enough gas" in {
      info("When the car is not starting and there is no gas information")
      val p = program + notStarting

      val model = evaluation(p)

      assert(model.head.contains(G_not))
    }

    it should "result in a defect because of the ignition" in {
      info("When the car is not starting and there is a broken ignition")
      val p = program + notStarting + brokenIgnition

      val model = evaluation(p)

      assert(model.head.contains(D))
    }

    it should "result in a defect because of the ign" in {
      info("When the car is not starting and there is a broken ignition and enough gas")
      val p = program + notStarting + enoughGas + brokenIgnition

      val model = evaluation(p)

      assert(model.head.contains(D))
    }
  }
}

class Car extends FlatSpec with CarBehavior with EvaluateJtmsImplementations{
  "The car sample" should behave like theSame(theCar)
}

