package jtms.asp.examples

import core._
import core.asp.AspRule
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
trait JTMS_21Behavior_ASP extends JTMSSpecASP {
  this: FlatSpec =>

  val Falsum = new ContradictionAtom("f")
  val j7: AspRule = AspRule(Falsum,Set(b),Set(c))

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

class JTMS_21_ASP extends JTMSSpecASP with JTMS_21Behavior_ASP with EvaluateASPImplementations {
  "The example 21" should behave like theSame(example21)

}
