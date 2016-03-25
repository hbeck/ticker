package jtms.tmn.founding

import core.{Program, Rule, Atom}
import jtms.TMNRefactored
import org.scalatest.FlatSpec

/**
  * Created by FM on 02.03.16.
  */
class TwoNegationNodes extends FlatSpec {
  val a = Atom("a")
  val b = Atom("b")

  val r1 = Rule.neg(a).head(b)
  val r2 = Rule.neg(b).head(a)

  val program = Program(r1, r2)

  def tmn = TMNRefactored(program)

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

  /* TODO
  "The model a" should "be founded" in {
    assert(modelA.isFounded(modelA.getModel.get))
  }
  "The model b" should "be founded" in {
    assert(modelB.isFounded(modelB.getModel.get))
  }
  */
}
