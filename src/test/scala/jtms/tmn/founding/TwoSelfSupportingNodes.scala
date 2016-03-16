package jtms.tmn.founding

//import core.{Fact, Program, Rule, Atom}
import common.sets.minus
import core.{Program, Rule, Atom}
import jtms.{out, TMN}
import org.scalatest.FlatSpec

/**
  * Created by FM on 02.03.16.
  */
class TwoSelfSupportingNodes extends FlatSpec {
  val a = Atom("a")
  val b = Atom("b")

  val b_if_a = Rule(b,a)
  val a_if_b = Rule(a,b)

  //stepwise:

  val tmn0 = TMN()

  val m1 = tmn0.getModel().get
  tmn0.add(b_if_a)
  val m2 = tmn0.getModel().get
  val addedR1 = minus(m2,m1)

  assert(tmn0.status(a) == out)
  assert(tmn0.status(b) == out)
  assert(m1.isEmpty)
  assert(addedR1.isEmpty)

  tmn0.add(a_if_b)
  val m3 = tmn0.getModel().get
  val addedR2 = minus(m3,m2)
  assert(tmn0.status(a) == out)
  assert(tmn0.status(b) == out)
  assert(m3.isEmpty)
  assert(addedR2.isEmpty)

  //all at once:
  val program = Program(b_if_a, a_if_b)
  val tmn = TMN(program)

  "A program containing only two self supporting nodes" should "have no model" in {
    println(tmn.getModel.get)
    assert(tmn.getModel.get == Set[Atom]())
  }

  //TODO (hb): use isFounded on specific list
//  it should "not mark the model a, b as founded" in {
//    assert(tmn.isFounded(Set(a, b)) == false)
//  }
//  it should "mark the empty model as founded" in {
//    assert(tmn.isFounded(Set()))
//  }

}