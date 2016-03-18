package jtms.tmn.examples

import asp.Asp
import aspsamples.EvaluateBothImplementations
import core._
import jtms.TMN
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
trait JTMS_21Behavior extends JTMSSpec {
  this: FlatSpec =>

  val Falsum = new ContradictionAtom("f")
  val j7: Rule = Rule.pos(b).neg(c).head(Falsum)

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

class JMTS_21 extends JTMSSpec with JTMS_21Behavior with EvaluateBothImplementations {
  "The example 21" should behave like theSame(example21)

}
