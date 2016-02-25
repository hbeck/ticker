package jtms.tmn.examples

import aspsamples.EvaluateBothImplementations
import core._
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

  val N_cont = ContradictionAtom("contradiction")

  val j0 = Rule.in(S_not).out(D).head(G_not);
  val j1 = Rule.in(S_not, G).head(D)
  val j2 = Rule.in(G, G_not).head(N_cont)
  val j3 = Rule.in(I).head(D)
  val j4 = Rule.in(C).head(D)

  val notStarting = Premise(S_not)
  val enoughGas = Premise(G)
  val notEnoughGas = Premise(G_not)
  val brokenIgnition = Premise(C)

  val program = Program(j0, j1, j2, j3, j4)

  def theCar(evaluation: Evaluation): Unit = {
    it should "not result in a defect" in {
      info("When the car is not starting and there is not enough gas")
      val p = program + notStarting + notEnoughGas

      val model = evaluation(p)

      assert(model.get.contains(D) == false)
    }

    it should "result in a defect" in {
      info("When the car is not starting and there is enough gas")
      val p = program + notStarting + enoughGas

      val model = evaluation(p)

      assert(model.get.contains(D))
    }

    it should "result in not enough gas" in {
      info("When the car is not starting and there is no gas information")
      val p = program + notStarting

      val model = evaluation(p)

      assert(model.get.contains(G_not))
    }

    it should "result in a defect because of the ignition" in {
      info("When the car is not starting and there is a broken ignition")
      val p = program + notStarting + brokenIgnition

      val model = evaluation(p)

      assert(model.get.contains(D))
    }

    it should "result in a defect because of the ign" in {
      info("When the car is not starting and there is a broken ignition and enough gas")
      val p = program + notStarting + enoughGas + brokenIgnition

      val model = evaluation(p)

      assert(model.get.contains(D))
    }
  }
}

class Car extends FlatSpec with CarBehavior with EvaluateBothImplementations{
  "The car sample" should behave like theSame(theCar)
}

