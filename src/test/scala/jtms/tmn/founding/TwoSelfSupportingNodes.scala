package jtms.tmn.founding

import core.{Fact, Program, Rule, Atom}
import jtms.TMN
import org.scalatest.FlatSpec

/**
  * Created by FM on 02.03.16.
  */
class TwoSelfSupportingNodes extends FlatSpec {
  val a = Atom("a")
  val b = Atom("b")

  val r1 = Rule.pos(a).head(b)
  val r2 = Rule.pos(b).head(a)

  val program = Program(r1, r2)

  val tmn = TMN(program)


  "A program containing only two self supporting nodes" should "have no model" in {
    assert(tmn.getModel == None)
  }

  it should "not mark the model a, b as founded" in {
    assert(tmn.isFounded(Set(a, b)) == false)
  }
  it should "mark the empty model as founded" in {
    assert(tmn.isFounded(Set()))
  }

}