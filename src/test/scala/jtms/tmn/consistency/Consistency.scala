package jtms.tmn.consistency

import core.{Atom, ContradictionAtom, Rule}
import jtms.TMN
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class Consistency extends FunSuite {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
//  val d = Atom("d")
//  val e = Atom("e")
//  val f = Atom("f")

  val n = ContradictionAtom("n")

  val none = Set[Atom]()

  //base case for next (no ddb here)
  test("a") {
    val tmn = TMN()
    tmn.add(Rule(a))
    val model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains a)
  }

  test("a :- not b. then b.") {
    val tmn = TMN()
    tmn.add(Rule(a,none,Set(b)))
    var model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains a)

    tmn.add(Rule(b,none,none))
    model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains b)
  }

  test("a or b. then not a.") {
    val tmn = TMN()
    tmn.add(Rule(a,none,Set(b)))
    tmn.add(Rule(b,none,Set(a)))
    var model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains a) //from add order

    tmn.add(Rule(n,a)) // :- a
    model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains b)
  }

  test("a :- c. c :- a. b :- not a. :- b, not c.") {
    val tmn = TMN()
    tmn.add(Rule(a,c))

    assert(tmn.getModel.get.isEmpty)

    tmn.add(Rule(c,a))
    assert(tmn.getModel.get.isEmpty)

    tmn.add(Rule(b,none,Set(a)))
    assert(tmn.getModel.get.size==1)
    assert(tmn.getModel.get contains b)

    tmn.add(Rule(n,Set(b),Set(c)))
    assert(tmn.getModel == None)
  }

  //inconsistent
  test(":- not a") {
    val tmn = TMN()
    tmn.add(Rule(n,none,Set(a)))
    assert(tmn.getModel == None)
  }

  //inconsistent
  test("a. :- a.") {
    val tmn = TMN()
    tmn.add(Rule(a))
    tmn.add(Rule(n,a))
    assert(tmn.getModel == None)
  }

}
