package jtms.tmn.examples

import common.sets.symmdiff
import core.{Atom, Fact}

/**
  * Created by FM on 06.02.16.
  */
class JTMS_5 extends JTMSSpec {

  val j0 = Fact(A)

  var diff: Set[Atom] = Set()

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
    assert(tmn.getModel().get == Set(A, C, D, E, F))
  }
  it should "have state changes in A,B,C,F" in {
    assert(diff == Set(A, B, C, F))
  }
}
