package jtms.asp.examples

import core.Atom
import core.asp.{AspFact, AspRule}
import fixtures.AtomTestFixture
import jtms.ExtendedJtms
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class ASPConsistency extends FunSuite with AtomTestFixture{

  val none = Set[Atom]()

  test("a") {

    val tms = ExtendedJtms()
    def m = tms.getModel

    assert(m.get.isEmpty)

    tms.add(AspFact(a))
    assert(m.get == Set(a))

    tms.remove(AspFact(a))
    assert(m.get.isEmpty)
  }

  test("a :- not b. then b.") {

    val tms = ExtendedJtms()
    def m = tms.getModel

    tms.add(AspRule(a,none,Set(b)))
    assert(m.get == Set(a))

    tms.add(AspFact(b))
    assert(m.get == Set(b))

    tms.remove(AspFact(b))
    assert(m.get == Set(a))
  }

  test("a :- not b. b :- not a.  b.") {

    val tms = ExtendedJtms()
    def m = tms.getModel

    tms.add(AspRule(a,none,Set(b)))
    tms.add(AspRule(b,none,Set(a)))
    assert(m.get == Set(a))

    tms.add(AspFact(b))
    assert(m.get == Set(b))

    tms.remove(AspFact(b))
    assert(m.get == Set(a) || m.get == Set(b)) //!
  }

  //consistent: 'inactive' odd loop
  test("a. a :- not a.") {

    var tms = ExtendedJtms()
    def m = tms.getModel

    tms.add(AspFact(a))
    tms.add(AspRule(a,Set(),Set(a)))
    assert(m.get == Set(a))

    tms = ExtendedJtms()
    tms.add(AspRule(a,Set(),Set(a)))
    tms.add(AspFact(a))
    assert(m.get == Set(a))
  }

  test("inc1: a :- not b. b :- a") {

    val tms = ExtendedJtms()
    def m = tms.getModel

    tms.add(AspRule(a,none,Set(b)))
    assert(m.get == Set(a))

    tms.add(AspRule(b,a))
    assert(m == None)

    tms.remove(AspRule(b,a))
    assert(m.get == Set(a))
    
  }

  test("inc2: a :- not b. b :- not c. c :- not a") {

    val tms = ExtendedJtms()
    def m = tms.getModel

    tms.add(AspRule(a,none,Set(b)))
    assert(m.get == Set(a))

    tms.add(AspRule(b,none,Set(c)))
    assert(m.get == Set(b))

    tms.add(AspRule(c,none,Set(a)))
    assert(m == None)

    tms.remove(AspRule(c,none,Set(a)))
    assert(m.get == Set(b))

  }

  test("inc3: a :- d, not b, not c. b :- a, d. d :- not e. e.") {
    val tms = ExtendedJtms()
    def m = tms.getModel

    tms.add(AspRule(a,Set(d),Set(b,c)))
    assert(m.get == Set())

    tms.add(AspRule(b,Set(a,d)))
    assert(m.get == Set())

    tms.add(AspRule(d,none,Set(e)))
    assert(m == None)

    tms.add(AspFact(e))
    assert(m.get == Set(e))

    tms.remove(AspFact(e))
    assert(m == None)
  }

  //inconsistent: indirect odd loop
//  test("a :- b. b :- c. c :- not a.") {
//    val tms = AnswerUpdateNetwork()
//    tms.add(Rule(a, b))
//    tms.add(Rule(b, c))
//    tms.add(Rule(c, none, Set(a)))
//    assert(tms.getModel == None)
//
//    tms.add(Rule(c))
//    assert(tms.getModel.get == Set[Atom](a, b, c))
//  }

  //inconsistent: indirect odd loop
//  test("a :- b. b :- c. c :- d. d :- not a.") {
//    val tms = AnswerUpdateNetwork()
//    tms.add(Rule(a, b))
//    tms.add(Rule(b, c))
//    tms.add(Rule(c, d))
//    tms.add(Rule(d, none, Set(a)))
//    assert(tms.getModel == None)
//
//    tms.add(Rule(d))
//    assert(tms.getModel.get == Set[Atom](a, b, c, d))
//  }

}
