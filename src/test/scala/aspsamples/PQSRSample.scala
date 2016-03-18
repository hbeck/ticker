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

  val Falsum = new ContradictionAtom("n")

  val programSFirst = Program(
    Rule(p,Set(q),Set(s)),
    Rule(r,Set(p),Set(q,s)),
    Rule(s,none,Set(q)), //s
    Rule(q,none,Set(s)) //q
  )

  val programQFirst = Program(
    Rule(p,Set(q),Set(s)),
    Rule(r,Set(p),Set(q,s)),
    Rule(q,none,Set(s)), //q
    Rule(s,none,Set(q)) //s
  )

  def generateTwoModels(evaluation: Evaluation) = {
    it should "generate the model s" in {
      val model = evaluation(programSFirst)
      assert(model contains Set(s))
    }
    it should "generate the model p,q" in {
      val model = evaluation(programQFirst)
      assert(model contains Set(p,q))
    }
  }

  def withKillClause(evaluation: Evaluation) = {
    val c = ContradictionAtom("c")
    val p = programQFirst + Rule(c,Set(q),Set(r)) //Q first!

    it should "generate only one model" in {
      val model = evaluation(p)

      assert(model contains Set(s))
    }
  }

  "Two models" should behave like theSame(generateTwoModels)

  "With a kill clause" should behave like theSame(withKillClause)
}
