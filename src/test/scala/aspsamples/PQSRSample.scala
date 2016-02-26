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

  val cont = ContradictionAtom("cont")

  val program = Program(
    Rule.in(q).out(s).head(p),
    Rule.in(p).out(q, s).head(r),
    Rule.out(q).head(s),
    Rule.out(s).head(q)
  )

  def generateTwoModels(evaluation: Evaluation) = {
    it should "generate the model s" in {
      val model = evaluation(program)

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
    val p = program + Rule.in(q).out(r).head(cont)

    it should "generate only one model" in {
      val model = evaluation(p)

      assert(model contains SingleModel(Set(s)))
    }
  }

  "Two models" should behave like theSame(generateTwoModels)

  "With a kill clause" should behave like theSame(withKillClause)
}
