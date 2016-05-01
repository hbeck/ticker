package jtms.asp.examples

import common.sets.symmdiff
import core.Atom
import core.asp.AspFact

/**
  * Created by FM on 06.02.16.
  */
class JTMS_5_ASP extends JTMSSpecASP {

  val j0 = AspFact(a)

  var diff: Set[Atom] = Set()

  val net = {
    val network = Network
    network.set(Set(e, b, d))
    val m1 = network.getModel.get
    network.add(j0)
    val m2 = network.getModel.get
    diff = symmdiff(m1,m2)
    network
  }

  "The new model" should "be A,C,D,E,F" in {
    assert(net.getModel().get == Set(a, c, d, e, f))
  }
  it should "have state changes in A,B,C,F" in {
    assert(diff == Set(a, b, c, f))
  }
}
