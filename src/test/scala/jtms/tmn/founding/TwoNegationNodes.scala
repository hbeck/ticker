package jtms.tmn.founding

import core.asp.{AspProgram, AspRule}
import fixtures.AtomTestFixture
import jtms.algorithms.Jtms
import org.scalatest.FlatSpec

/**
  * Created by FM on 02.03.16.
  */
class TwoNegationNodes extends FlatSpec with AtomTestFixture {

  val r1 = AspRule.neg(a).head(b)
  val r2 = AspRule.neg(b).head(a)

  val program = AspProgram(r1, r2)

  def tmn = Jtms(program)

  val modelA = {
    val t = tmn
    t.set(Set(a))
    t
  }

  val modelB = {
    val t = tmn
    t.set(Set(b))
    t
  }

  "Two supporting, negative atoms" should "have the valid model a" in {
    assert(modelA.getModel.get == Set(a))
  }

  it should "have the valid model b" in {
    assert(modelB.getModel.get == Set(b))
  }

}
