package jtms.tmn.examples

import core.{Premise, Atom}

/**
  * Created by FM on 06.02.16.
  */
class JTMS_5 extends JTMSSpec {

  val j0 = Premise(a)

  var diff: Set[Atom] = Set();
  val tmn = {
    val tmn = JTMS
    tmn.set(Set(e, b, d))
    diff = tmn.add(j0)
    tmn
  }

  "The new model" should "be A,C,D,E,F" in {
    assert(tmn.getModel() == Set(a, c, d, e, f))
  }
  it should "have state changes in A,B,C,F" in {
    assert(diff == Set(a, b, c, f))
  }
}
