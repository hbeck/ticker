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

  val Falsum = new ContradictionAtom("f")

  val j0 = Rule.pos(P).head(F_not)
  val j1 = Rule.pos(P).head(V)
  val j2 = Rule.pos(V).neg(F_not).head(F)
  val j3 = Rule.pos(F, F_not).head(Falsum)
  val j4 = Fact(V)

  val j5 = Fact(P)

  val program = Program(j0, j1, j2, j3, j4)

  def tweety(evaluation: Evaluation) = {
    it should "contain only V and F" in {
      info("The initial model")
      assert(evaluation(program) contains Set(V, F))
    }

    it should "result in a new Model containing V, P and F_not" in {
      info("Adding a new Premise P")
      val p = program + j5

      assert(evaluation(p) contains Set(V, P, F_not))
    }
  }
}

class Tweety extends FlatSpec with TweetyBehavior with EvaluateBothImplementations {
  "The Tweety" should behave like theSame(tweety)
}
