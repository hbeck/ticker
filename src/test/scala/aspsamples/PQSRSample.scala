package aspsamples

import core._
import org.scalatest.FlatSpec

/**
  * Created by FM on 26.02.16.
  */

class PQSRSample extends FlatSpec with EvaluateBothImplementations {
  //  this: FlatSpec =>

  val p = Atom("p")
  val q = Atom("q")
  val s = Atom("s")
  val r = Atom("r")

  val program = Program(
    Rule.pos(q).neg(s).head(p),
    Rule.pos(p).neg(q, s).head(r),
    Rule.neg(q).head(s),
    Rule.neg(s).head(q)
  )

  def generateTwoModels(evaluation: Evaluation) = {
    it should "generate the model s" in {
      val model = evaluation(program)

      //TODO (HB): question about containment
      assert(model.get.contains(Set(s)))
    }
    it should "generate the model p,q" in {
      val model = evaluation(program)

      if (model.get.isInstanceOf[SingleModel])
        pending
      assert(model.get.contains(Set(p, q)))
    }
  }

  def withKillClause(evaluation: Evaluation) = {
    val p = program + Constraint.pos(q).neg(r)

    it should "generate only one model" in {
      val model = evaluation(p)

      assert(model contains SingleModel(Set(s)))
    }
  }

  "Two models" should behave like theSame(generateTwoModels)

  "With a kill clause" should behave like theSame(withKillClause)
}
