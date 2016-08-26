package jtms.tmn.examples

import core._
import core.asp.{AspRule, NormalRule}
import jtms.asp.examples.EvaluateJtmsImplementations
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
trait Jtms_21_Behavior extends JtmsSpec {
  this: FlatSpec =>

  val Falsum = new ContradictionAtom(Predicate("f"))
  val j7: NormalRule = AspRule.pos(b).neg(c).head(Falsum)

  def p = {
    val p = program + j7
    p
  }

  def example21(evaluation: Evaluation): Unit = {
    it should "not generate a model" in {
      intercept[RuntimeException] {
        val model = evaluation(p)
        assert(model == None)
      }


      // this is not a founded/sounded model!
      // a and c are supporting each other
      // -> do a check on the final model
      // -> we need do do DDB with all variants
      // --> if there is no model with any possible enumeration -> fail


    }
  }
}

class Jtms_21_Spec extends JtmsSpec with Jtms_21_Behavior with EvaluateJtmsImplementations {
  "The example 21" should behave like theSame(example21)

}
