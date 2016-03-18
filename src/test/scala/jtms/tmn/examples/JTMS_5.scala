package jtms.tmn.examples

import core.{Fact, Atom}

/**
  * Created by FM on 06.02.16.
  */
class JTMS_5 extends JTMSSpec {

  val j0 = Fact(a)

  var diff: Set[Atom] = Set();
  val tmn = {
    val tmn = JTMS
    tmn.set(Set(E, B, D))
    val m1 = tmn.getModel().get
    tmn.add(j0)
    val m2 = tmn.getModel().get
    diff = symmdiff(m1,m2)
    tmn
  }

  "The new model" should "be A,C,D,E,F" in {
    assert(tmn.getModel().get == Set(a, c, d, e, f))
  }
  it should "have state changes in A,B,C,F" in {
    assert(diff == Set(a, b, c, f))
  }
}
