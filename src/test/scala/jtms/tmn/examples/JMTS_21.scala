package jtms.tmn.examples

import asp.Asp
import aspsamples.EvaluateBothImplementations
import core._
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
trait JTMS_21Behavior extends JTMSSpec {
  this: FlatSpec =>

  val j7 = Constraint.pos(b).neg(c)

  def p = {
    val p = program + j7
    p
  }

  def example21(evaluation: Evaluation): Unit = {
    it should "contain A,C,D,F,E" in {
      val model = evaluation(p)

      // this is not a founded/sounded model!
      // a and c are supporting each other
      // -> do a check on the final model
      // -> we need do do DDB with all variants
      // --> if there is no model with any possible enumeration -> fail
      if (evaluation.isInstanceOf[Asp])
        pending
      assert(model contains SingleModel(Set(a, c, d, f, e)))
    }
  }
}

class JMTS_21 extends JTMSSpec with JTMS_21Behavior with EvaluateBothImplementations {
  "The example 21" should behave like theSame(example21)

}
