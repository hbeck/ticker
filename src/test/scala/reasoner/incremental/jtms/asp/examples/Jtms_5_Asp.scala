package reasoner.incremental.jtms.asp.examples

import common.sets.symmdiff
import core.Atom
import core.asp.AspFact

/**
  * Created by FM on 06.02.16.
  */
class Jtms_5_Asp extends JtmsSpecAsp {

  val j0 = AspFact(a)

  var diff: Set[Atom] = Set()

  val jtms = {
    val jtms = JTMS
    jtms.set(Set(e, b, d))
    val m1 = jtms.getModel.get
    jtms.add(j0)
    val m2 = jtms.getModel.get
    diff = symmdiff(m1,m2)
    jtms
  }

  "The new model" should "be A,C,D,E,F" in {
    assert(jtms.getModel().get == Set(a, c, d, e, f))
  }
  it should "have state changes in A,B,C,F" in {
    assert(diff == Set(a, b, c, f))
  }
}
