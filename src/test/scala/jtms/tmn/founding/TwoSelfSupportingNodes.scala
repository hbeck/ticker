package jtms.tmn.founding

import core.asp.{AspProgram, AspRule}
import fixtures.AtomTestFixture
import jtms.algorithms.{Jtms, JtmsDoyle}
import org.scalatest.FlatSpec


/**
  * Created by FM on 02.03.16.
  */
class TwoSelfSupportingNodes extends FlatSpec with AtomTestFixture {

  val r1 = AspRule.pos(a).head(b)
  val r2 = AspRule.pos(b).head(a)

  val program = AspProgram(r1, r2)

  val tmn = Jtms(program)

  "A program containing only two self supporting nodes" should "have no model" in {
    assert(tmn.getModel.get == Set())
  }


}