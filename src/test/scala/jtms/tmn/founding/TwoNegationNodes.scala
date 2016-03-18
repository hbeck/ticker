package jtms.tmn.founding

import core.{Program, Rule, Atom}
import jtms.TMN
import org.scalatest.FlatSpec

/**
  * Created by FM on 02.03.16.
  */
class TwoNegationNodes extends FlatSpec {
  val a = Atom("a")
  val b = Atom("b")

  val none = Set[Atom]()

  val ra = Rule(a,none,Set(b))
  val rb = Rule(b,none,Set(a))

  val program = Program(ra, rb)

  def tmn = TMN(program)

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

  "The model a" should "be founded" in {
    assert(modelA.isFounded(modelA.getModel.get))
  }
  "The model b" should "be founded" in {
    assert(modelB.isFounded(modelB.getModel.get))
  }
}
