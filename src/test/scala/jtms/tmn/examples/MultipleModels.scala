package jtms.tmn.examples

import asp.Asp
import core._
import jtms.asp.examples.EvaluateJTMNImplementations
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
trait MultipleModelsBehavior {
  this: FlatSpec =>

  val a = Atom("a")
  val b = Atom("b")

  val j1 = AspRule.neg(a).head(b)
  val j2 = AspRule.neg(b).head(a)

  def multipleModels(evaluation: Evaluation) = {

    it should "be B" in {
      info("When adding j1 before j2 the valid model")
      val model = evaluation(AspProgram(j1, j2))

      if (evaluation.isInstanceOf[Asp])
        assert(model == Set(Set(b), Set(a)))
      else {
        assert(model contains Set(b))
        //pending
      }
    }

    it should "be A" in {
      info("When adding j2 before j1 the valid model")
      val model = evaluation(AspProgram(j2, j1))
      if (evaluation.isInstanceOf[Asp])
        assert(model == Set(Set(b), Set(a)))
      else {
        assert(model contains Set(a))
        //pending
      }
    }
  }
}

class MultipleModels extends FlatSpec with MultipleModelsBehavior with EvaluateJTMNImplementations {
  "Multiple Models" should behave like theSame(multipleModels)
}
