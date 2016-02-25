package jtms.tmn.examples

import aspsamples.EvaluateBothImplementations
import core._
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
trait TweetyBehavior {
  this: FlatSpec =>
  val V = Atom("vogel_sein")
  val P = Atom("pinguin_sein")
  val F = Atom("fliegen_koennen")
  val F_not = Atom("nicht_fliegen_koennen")
  val N_cont = ContradictionAtom("widerspruch")

  val j0 = Rule.in(P).head(F_not)
  val j1 = Rule.in(P).head(V)
  val j2 = Rule.in(V).out(P).head(F)
  val j3 = Rule.in(F, F_not).head(N_cont)
  val j4 = Premise(V)

  val j5 = Premise(P)

  val program = Program(j0, j1, j2, j3, j4)

  def tweety(evaluation: Evaluation) = {
    it should "contain only V and F" in {
      info("The initial model")
      assert(evaluation(program) contains SingleModel(Set(V, F)))
    }

    it should "result in a new Model containing V, P and F_not" in {
      info("Adding a new Premise P")
      val p = program + j5

      assert(evaluation(p) contains SingleModel(Set(V, P, F_not)))
    }
  }
}

class Tweety extends FlatSpec with TweetyBehavior with EvaluateBothImplementations {
  "The Tweety" should behave like theSame(tweety)
}
