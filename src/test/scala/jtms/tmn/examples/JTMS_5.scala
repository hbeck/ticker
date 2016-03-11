package jtms.tmn.examples

import core.{Fact, Atom}

/**
  * Created by FM on 06.02.16.
  */
class JTMS_5 extends JTMSSpec {

  val j0 = Fact(A)

  var diff: Option[Set[Atom]] = Some(Set())
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
