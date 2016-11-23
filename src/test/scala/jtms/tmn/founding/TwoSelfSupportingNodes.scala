package jtms.tmn.founding

import core.asp.{AspProgram, AspRule}
import fixtures.AtomTestFixture
import jtms.algorithms.JtmsDoyle
import org.scalatest.FlatSpec


/**
  * Created by FM on 02.03.16.
  */
class TwoSelfSupportingNodes extends FlatSpec with AtomTestFixture {

  val r1 = AspRule.pos(a).head(b)
  val r2 = AspRule.pos(b).head(a)

  val program = AspProgram(r1, r2)

  val tmn = JtmsDoyle(program) //TODO

  "A program containing only two self supporting nodes" should "have no model" in {
    assert(tmn.getModel.get == Set())
  }

  /* TODO
  it should "not mark the model a, b as founded" in {
    assert(tmn.isFounded(Set(a, b)) == false)
  }
  it should "mark the empty model as founded" ignore  {
    assert(tmn.isFounded(Set()))
  }
  */
}