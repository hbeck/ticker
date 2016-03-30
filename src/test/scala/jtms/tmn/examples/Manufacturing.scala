package jtms.tmn.examples

import aspsamples.EvaluateJTMNImplementations
import core._
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
trait ManufacturingBehavior {
  this: FlatSpec =>

  val C = Atom("product")
  val B = Atom("troubles")
  val A1 = Atom("resource_1")
  val A2 = Atom("resource_2")
  val L1 = Atom("supply_problems_A1")

  val j0 = Rule.pos(C).neg(B).head(A1)
  val j1 = Rule.pos(C, B).head(A2)
  val j2 = Rule.pos(L1).head(B)
  val j3 = Rule.fact(C)

  val program = Program(j0, j1, j2, j3)

  def manufacturing(evaluation: Evaluation) = {
    it should "use resource A1" in {
      info("When manufacturing without troubles")
      val model = evaluation(program)

      assert(model contains Set(C, A1))
    }

    it should "mark as troubles and use resource A2" in {
      info("When there are supply problems with A1")
      val p = program + Fact(L1)

      val model = evaluation(p)

      assert(model contains Set(C, L1, B, A2))
    }
  }

}

class Manufacturing extends FlatSpec with ManufacturingBehavior with EvaluateJTMNImplementations {
  "The Manufacturing sample" should behave like theSame(manufacturing)
}