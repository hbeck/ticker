package jtms.tmn.examples

import core.ContradictionAtom
import core.asp.{AspFact, AspRule}

/**
  * Created by FM on 11.02.16.
  */
class Jtms_21 extends JtmsSpec {
  val N_contr = ContradictionAtom("N_contr")

  val j7 = AspRule(N_contr, Set(b), Set(c))

  def JTMS_DDB = {
    val tmn = JTMS

    tmn.add(j7)

    tmn
  }

  def JTMS_DDB_addA = {
    val tmn = JTMS_DDB
    tmn.add(AspFact(a))
    tmn
  }

  "The model" should "be inconsistent" in {
    assert(JTMS_DDB.getModel().get == Set(a,c,d,e,f)) //diff to ASP, which has None
  }

  "The model" should "contain A,C,D,F,E" in {
    assert(JTMS_DDB_addA.getModel().get == Set(a, c, d, f, e))
  }
}
