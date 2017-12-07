package experimental.jtms

import core._
import core.asp.{AspRule, NormalRule}
import jtms.asp.examples.{EvaluateAspImplementations, JtmsSpecAsp}
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
trait Jtms_21_Behavior_Asp extends JtmsSpecAsp {
  this: FlatSpec =>

  val Falsum = new ContradictionAtom(Predicate("f"))
  val j7: NormalRule = AspRule(Falsum,Set(b),Set(c))

  val p = program + j7

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

class Jtms_21_Asp extends JtmsSpecAsp with Jtms_21_Behavior_Asp with EvaluateAspImplementations {
  "The example 21" should behave like theSame(example21)

}
