package jtms.asp.examples

import core._
import core.asp.{AspProgram, AspRule}
import org.scalatest.FlatSpec

/**
  * Created by FM on 26.02.16.
  */

class PQSRSample extends FlatSpec with EvaluateAspImplementations {
  //  this: FlatSpec =>

  val none = Set[Atom]()

  val p = Atom("p")
  val q = Atom("q")
  val s = Atom("s")
  val r = Atom("r")

  val programSFirst = AspProgram(
    AspRule(p, Set(q), Set(s)),
    AspRule(r, Set(p), Set(q, s)),
    AspRule(s, none, Set(q)), //s
    AspRule(q, none, Set(s)) //q
  )

  val programQFirst = AspProgram(
    AspRule(p, Set(q), Set(s)),
    AspRule(r, Set(p), Set(q, s)),
    AspRule(q, none, Set(s)), //q
    AspRule(s, none, Set(q)) //s
  )

  def generateTwoModels(evaluation: Evaluation) = {
    it should "generate the model s" in {
      val model = evaluation(programSFirst)
      assert(model contains Set(s))
    }
    it should "generate the model p,q" in {
      val model = evaluation(programQFirst)
      assert(model contains Set(p, q))
    }
  }

  //constraints not implemented
//  def withKillClause(evaluation: Evaluation) = {
//    val c = ContradictionAtom(Predicate("c"))
//    val p = programQFirst + Rule(c, Set(q), Set(r)) //Q first!
//
//    it should "generate only one model" in {
//      val model = evaluation(p)
//
//      assert(model contains Set(s))
//    }
//  }

  "Two models" should behave like theSame(generateTwoModels)

  //"With a kill clause" should behave like theSame(withKillClause)
}
