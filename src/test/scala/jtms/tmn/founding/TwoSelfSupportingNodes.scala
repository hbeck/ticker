package jtms.tmn.founding

//import core.{Fact, Program, Rule, Atom}
import core.{Program, Rule, Atom}
import jtms.{out, TMN}
import org.scalatest.FlatSpec

/**
  * Created by FM on 02.03.16.
  */
class TwoSelfSupportingNodes extends FlatSpec {
  val a = Atom("a")
  val b = Atom("b")

  val r1 = Rule(b,a)
  val r2 = Rule(a,b)

  val program = Program(r1, r2)

  //stepwise:

  val tmn0 = TMN()

  val addedR1: Option[Set[Atom]] = tmn0.add(r1)
  assert(tmn0.status(a) == out)
  assert(tmn0.status(b) == out)
  assert(addedR1.get.isEmpty)
  assert(tmn0.model.get==Set[Atom]())

  val addedR2: Option[Set[Atom]] = tmn0.add(r2)
  assert(tmn0.status(a) == out)
  assert(tmn0.status(b) == out)
  assert(addedR2.get.isEmpty)
  assert(tmn0.model.get==Set[Atom]())

  //all at once:

  val tmn = TMN(program)

  "A program containing only two self supporting nodes" should "have no model" in {
    println(tmn.model.get)
    assert(tmn.model.get == Set[Atom]())
  }

  //TODO (hb): use isFounded on specific list
//  it should "not mark the model a, b as founded" in {
//    assert(tmn.isFounded(Set(a, b)) == false)
//  }
//  it should "mark the empty model as founded" in {
//    assert(tmn.isFounded(Set()))
//  }

}