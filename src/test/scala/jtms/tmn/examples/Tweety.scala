package jtms.tmn.examples

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

  val j0 = Justification.in(P).head(F_not)
  val j1 = Justification.in(P).head(V)
  val j2 = Justification.in(V).out(P).head(F)
  val j3 = Justification.in(F, F_not).head(N_cont)
  val j4 = Premise(V)

  val j5 = Premise(P)

  def TMN = {
    val tmn = new TMN(Set(V, P, F, F_not, N_cont))

    tmn.add(j0)
    tmn.add(j1)
    tmn.add(j2)
    tmn.add(j3)
    tmn.add(j4)

    tmn
  }

  "The initial model" should "contain only V and F" in {
    assert(TMN.getModel() == Set(V, F))
  }

  "Adding a new Premise P" should "result in a new Model containing V, P and F_not" in {
    val tmn = TMN

    tmn.add(j5)

    assert(tmn.getModel() == Set(V, P, F_not))
  }
}
