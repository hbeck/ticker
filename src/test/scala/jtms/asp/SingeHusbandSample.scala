package jtms.asp

import core.{Program, Rule, Premise, Atom}
import jtms.TMN
import org.scalatest.FlatSpec

/**
  * Created by FM on 25.02.16.
  */
class SingeHusbandSample extends FlatSpec {
  /*
  man.
single :- man, not husband.
husband :- man, not single.
   */

  val man = Atom("man")
  val single = Atom("single")
  val husband = Atom("husband")

  val r0 = Premise(man)
  val r1 = Rule.in(man).out(husband).head(single)
  val r2 = Rule.in(man).out(single).head(husband)

  val program = Program(r0, r1, r2)

  val tmn = TMN(program)

  "One model" should "include the model man, single" in {
    assert(tmn.getModel() == Set(man, single))
  }

  "The other model" should "include man, husband" in {
    pending
    assert(tmn.getModel() == Set(man, husband))
  }
}
