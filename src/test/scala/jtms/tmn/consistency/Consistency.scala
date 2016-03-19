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

  val times = 100

  test("a") {
    val tmn = TMN()
    var model = tmn.getModel.get
    assert(model.isEmpty)

    tmn.add(Rule(a))
    model = tmn.getModel.get
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

  test("a :- not b. b :- not a. n :- a.") {

    for (i <- 1 to times) {
      val tmn = TMN()
      tmn.add(Rule(a, none, Set(b)))
      tmn.add(Rule(b, none, Set(a))) //-> {a}

      var model = tmn.getModel.get
      assert(model == Set(a))

      tmn.add(Rule(n, a))
      model = tmn.getModel.get
      assert(model == Set(b))
    }
  }

  test("b :- not a. :- b, not c.") { //JTMS_21 base case
    for (i <- 1 to times) {
      val tmn = TMN()

      tmn.add(Rule(b, none, Set(a)))
      assert(tmn.getModel.get == Set(b))

      tmn.add(Rule(n, Set(b), Set(c)))
      assert(tmn.getModel == None)
    }
  }

  test("a :- c. c :- a. b :- not a. :- b, not c.") {
    for (i <- 1 to times) {
      val tmn = TMN()
      tmn.add(Rule(a, c))

      assert(tmn.getModel.get.isEmpty)

      tmn.add(Rule(c, a))
      assert(tmn.getModel.get.isEmpty)

      tmn.add(Rule(b, none, Set(a)))
      assert(tmn.getModel.get.size == 1)
      assert(tmn.getModel.get contains b)

      tmn.add(Rule(n, Set(b), Set(c)))
      assert(tmn.getModel == None)
    }
  }

  test("a :- c. c :- a. b :- not a. :- b, not c. add a before and after constraint.") {
    for (i <- 1 to times) {
      val tmnBefore = TMN()
      tmnBefore.add(Rule(a, c)) //{}
      tmnBefore.add(Rule(c, a)) //{}
      tmnBefore.add(Rule(b, none, Set(a))) //{b}
      //
      tmnBefore.add(Rule(a,none,none)) //{a,c}
      assert(tmnBefore.getModel.get == Set[Atom](a,c))
      tmnBefore.add(Rule(n, Set(b), Set(c))) //{a,c}
      assert(tmnBefore.getModel.get == Set[Atom](a,c))


      val tmnAfter = TMN()
      tmnAfter.add(Rule(a, c)) //{}
      tmnAfter.add(Rule(c, a)) //{}
      tmnAfter.add(Rule(b, none, Set(a))) //{b}
      //
      tmnAfter.add(Rule(n, Set(b), Set(c))) //None
      assert(tmnAfter.getModel == None)
      tmnAfter.add(Rule(a,none,none)) //{a,c}
      assert(tmnAfter.getModel.get == Set[Atom](a,c))

    }
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

  //consistent: 'inactive' odd loop
  test("a. a :- not a.") {
    /* TODO
    val tmnFactFirst = TMN()
    tmnFactFirst.add(Rule(a))
    tmnFactFirst.add(Rule(a,Set(),Set(a)))
    assert(tmnFactFirst.getModel.get == Set(a))

    val tmnRuleFirst = TMN()
    tmnRuleFirst.add(Rule(a,Set(),Set(a)))
    tmnRuleFirst.add(Rule(a))
    assert(tmnRuleFirst.getModel.get == Set(a)) */
  }

  //inconsistent: direct odd loop
  test("a :- not a.") {
    /* TODO
    val tmn = TMN()
    tmn.add(Rule(a,Set(),Set(a)))
    assert(tmn.getModel == None)
    */
  }

  //inconsistent: indirect odd loop
  test("a :- b. b :- c. c :- not a.") {
    /* TODO
    val tmn = TMN()
    tmn.add(Rule(a,b))
    tmn.add(Rule(b,c))
    tmn.add(Rule(c,none,Set(a)))
    assert(tmn.getModel == None) */
  }

}
