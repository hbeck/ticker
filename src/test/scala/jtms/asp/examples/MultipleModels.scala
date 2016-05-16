package jtms.asp.examples

import clingo.ClingoEvaluation
import core._
import core.asp.{AspProgram, AspRule}
import fixtures.AtomTestFixture
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
trait MultipleModelsBehavior extends AtomTestFixture {
  this: FlatSpec =>

  val j1 = AspRule.neg(a).head(b)
  val j2 = AspRule.neg(b).head(a)

  def multipleModels(evaluation: Evaluation) = {

    it should "be B" in {
      info("When adding j1 before j2 the valid model")
      val model = evaluation(AspProgram(j1, j2))

      if (evaluation.isInstanceOf[ClingoEvaluation])
        assert(model == Set(Set(b), Set(a)))
      else {
        assert(model contains Set(b))
        //pending
      }
    }

    it should "be A" in {
      info("When adding j2 before j1 the valid model")
      val model = evaluation(AspProgram(j2, j1))
      if (evaluation.isInstanceOf[ClingoEvaluation])
        assert(model == Set(Set(b), Set(a)))
      else {
        assert(model contains Set(a))
        //pending
      }
    }
  }
}

class MultipleModels extends FlatSpec with MultipleModelsBehavior with EvaluateASPImplementations {
  "Multiple Models" should behave like theSame(multipleModels)
}
