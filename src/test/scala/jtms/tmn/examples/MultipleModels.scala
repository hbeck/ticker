package jtms.tmn.examples

import asp.Asp
import aspsamples.EvaluateBothImplementations
import core._
import jtms.TMN
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
trait MultipleModelsBehavior {
  this: FlatSpec =>

  val a = Atom("a")
  val b = Atom("b")

  val j1 = Rule.neg(a).head(b)
  val j2 = Rule.neg(b).head(a)

  def multipleModels(evaluation: Evaluation) = {

    it should "be B" in {
      info("When adding j1 before j2 the valid model")
      val model = evaluation(Program(j1, j2))

      if (evaluation.isInstanceOf[Asp])
        assert(model contains MultipleModels(Set(Set(b), Set(a))))
      else {
        assert(model contains SingleModel(Set(b)))
        pending
      }
    }

    it should "be A" in {
      info("When adding j2 before j1 the valid model")
      val model = evaluation(Program(j2, j1))
      if (evaluation.isInstanceOf[Asp])
        assert(model contains MultipleModels(Set(Set(b), Set(a))))
      else {
        assert(model contains SingleModel(Set(a)))
        pending
      }
    }
  }
}

class MultipleModels extends FlatSpec with MultipleModelsBehavior with EvaluateBothImplementations {
  "Multiple Models" should behave like theSame(multipleModels)
}
