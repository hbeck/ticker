package jtms.tmn.examples

import core.{Premise, Atom}

/**
  * Created by FM on 06.02.16.
  */
class JTMS_5 extends JTMS {

  val j0 = Premise(A)

  var diff: Set[Atom] = Set();
  val tmn = {
    val tmn = JTMS
    tmn.set(Set(E, B, D))
    diff = tmn.add(j0)
    tmn
  }

  "The new model" should "be A,C,D,E,F" in {
    assert(tmn.getModel() == Set(A, C, D, E, F))
  }
  it should "have state changes in A,B,C,F" in {
    assert(diff == Set(A, B, C, F))
  }
}
