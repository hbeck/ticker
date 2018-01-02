package experimental.tms

import core.asp.{AspFact, AspRule}
import core.{ContradictionAtom, Predicate}
import reasoner.incremental.jtms.tmn.examples.JtmsSpec

/**
  * Created by FM on 11.02.16.
  */
class Jtms_21 extends JtmsSpec {
  val N_contr = ContradictionAtom(Predicate("N_contr"))

  val j7 = AspRule(N_contr, Set(b), Set(c))

  def JTMS_DDB = {
    val jtms = JTMS
    jtms.add(j7)
    jtms
  }

  def JTMS_DDB_addA = {
    val jtms = JTMS_DDB
    jtms.add(AspFact(a))
    jtms
  }

  "The model" should "be inconsistent" in {
    assert(JTMS_DDB.getModel().get == Set(a,c,d,e,f)) //diff to ASP, which has None
  }

  "The model" should "contain A,C,D,F,E" in {
    assert(JTMS_DDB_addA.getModel().get == Set(a, c, d, f, e))
  }
}
