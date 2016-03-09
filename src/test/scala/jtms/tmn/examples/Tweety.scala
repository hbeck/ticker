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
  val F = Atom("Fliegen können")
  val F_not = Atom("nicht fliegen können")
  val N_cont = ContradictionAtom("Widerspruch")

  val j0 = Rule.pos(P).head(F_not)
  val j1 = Rule.pos(P).head(V)
  val j2 = Rule.pos(V).neg(P).head(F)
  val j3 = Rule.pos(F, F_not).head(N_cont)
  val j4 = Fact(V)

  val j5 = Fact(P)

  val program = Program(j0, j1, j2, j3, j4)

  def Tmn = {
    val tmn = TMN(program)

    tmn
  }

  "The initial model" should "contain only V and F" in {
    assert(Tmn.getModel() == Set(V, F))
  }

  "Adding a new Premise P" should "result in a new Model containing V, P and F_not" in {
    val tmn = Tmn

    tmn.add(j5)

    assert(tmn.getModel() == Set(V, P, F_not))
  }
}
