package jtms.tmn.examples

import core._
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
class Tweety extends FlatSpec {

  val V = Atom("Vogel sein")
  val P = Atom("Pinguin sein")
  val F = Atom("Fliegen koennen")
  val F_not = Atom("nicht fliegen koennen")
  val N_contr = ContradictionAtom("Widerspruch")

  val j0 = Rule(F_not,P)
  val j1 = Rule(V,P)
  val j2 = Rule(F,Set(V),Set(P))
  val j3 = Rule(N_contr,Set(F, F_not))
  val j4 = Fact(V)

  val j5 = Fact(P)

  val program = Program(j0, j1, j2, j3, j4)

  def TweetyTMN = TMN(program)

  "The initial model" should "contain only V and F" in {
    assert(TweetyTMN.getModel().get == Set(V, F))
  }

  "Adding a new Premise P" should "result in a new Model containing V, P and F_not" in {
    val tmn = TweetyTMN

    tmn.add(j5)

    assert(tmn.getModel().get == Set(V, P, F_not))
  }
}
