package jtms.asp.examples

import core.asp.{AspProgram, AspRule}
import core.Atom
import jtms.ExtendedJTMS
import org.scalatest.FlatSpec

/**
  * Created by FM on 02.03.16.
  */
class TwoNegationNodes extends FlatSpec {
  val a = Atom("a")
  val b = Atom("b")

  val r1 = AspRule.neg(a).head(b)
  val r2 = AspRule.neg(b).head(a)

  val program = AspProgram(r1, r2)

  def net = ExtendedJTMS(program)

  val modelA = {
    val t = net

    t.set(Set(a))

    t
  }

  val modelB = {
    val t = net

    t.set(Set(b))

    t
  }

  "Two supporting, negative atoms" should "have the valid model a" in {
    assert(modelA.getModel.get == Set(a))
  }

  it should "have the valid model b" in {
    assert(modelB.getModel.get == Set(b))
  }

  /* TODO
  "The model a" should "be founded" in {
    assert(modelA.isFounded(modelA.getModel.get))
  }
  "The model b" should "be founded" in {
    assert(modelB.isFounded(modelB.getModel.get))
  }
  */
}
