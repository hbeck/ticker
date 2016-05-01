package jtms.asp.examples

import core.{Atom, ContradictionAtom, AspRule}
import jtms.ExtendedJTMS
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class ASPConsistency extends FunSuite {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")
//  val e = Atom("e")
  val f = Atom("f")

  val n = ContradictionAtom("n")

  val none = Set[Atom]()

  val times = 100

  test("a") {
    val net = ExtendedJTMS()
    assert(net.getModel.get.isEmpty)

    net.add(AspRule(a))
    assert(net.getModel.get == Set(a))

    net.remove(AspRule(a))
    assert(net.getModel.get.isEmpty)
  }

  test("a :- not b. then b.") {

    val net = ExtendedJTMS()
    net.add(AspRule(a,none,Set(b)))
    assert(net.getModel.get == Set(a))

    net.add(AspRule(b))
    assert(net.getModel.get == Set(b))

    net.remove(AspRule(b))
    assert(net.getModel.get == Set(a))
  }

  test("a :- not b. b :- not a.  b.") {

    val net = ExtendedJTMS()
    net.add(AspRule(a,none,Set(b)))
    net.add(AspRule(b,none,Set(a)))
    assert(net.getModel.get == Set(a))

    net.add(AspRule(b))
    assert(net.getModel.get == Set(b))

    net.remove(AspRule(b))
    assert(net.getModel.get == Set(a) || net.getModel.get == Set(b)) //!
  }

  //constraints not implemented
//  test("P1: a :- not b.  b :- not a.  n :- a.") {
//    val net = AnswerUpdateNetwork()
//    net.add(Rule(a, none, Set(b)))
//    net.add(Rule(b, none, Set(a))) //-> {a}
//
//    var model = net.getModel.get
//    assert(model == Set(a))
//
//    net.add(Rule(n, a))
//    model = net.getModel.get
//    assert(model == Set(b))
//  }

  //constraints not implemented
//  test("P2: b :- not a.  n :- b, not c.") { //JTMS_21 base case
//    val net0 = AnswerUpdateNetwork()
//
//    net0.add(Rule(n, Set(b), Set(c)))
//    assert(net0.getModel.get == Set[Atom]())
//
//    net0.add(Rule(b, none, Set(a)))
//    assert(net0.getModel == None)
//
//    val net1 = AnswerUpdateNetwork()
//
//    net1.add(Rule(b, none, Set(a)))
//    assert(net1.getModel.get == Set(b))
//
//    net1.add(Rule(n, Set(b), Set(c)))
//    assert(net1.getModel == None)
//  }

  //constraints not implemented
//  test("P3: a :- c.  c :- a.  b :- not a.  n :- b, not c.") {
//    val net = AnswerUpdateNetwork()
//    net.add(Rule(a, c))
//
//    assert(net.getModel.get.isEmpty)
//
//    net.add(Rule(c, a))
//    assert(net.getModel.get.isEmpty)
//
//    net.add(Rule(b, none, Set(a)))
//    assert(net.getModel.get.size == 1)
//    assert(net.getModel.get contains b)
//
//    net.add(Rule(n, Set(b), Set(c)))
//    assert(net.getModel == None)
//  }

  //constraints not implemented
//  test("P4:  a :- c.  c :- a.  b :- not a.  n :- b, not c.  a.") {
//    val netBefore = AnswerUpdateNetwork()
//    netBefore.add(Rule(a, c)) //{}
//    netBefore.add(Rule(c, a)) //{}
//    netBefore.add(Rule(b, none, Set(a))) //{b}
//    //
//    netBefore.add(Rule(a,none,none)) //{a,c}
//    assert(netBefore.getModel.get == Set[Atom](a,c))
//    netBefore.add(Rule(n, Set(b), Set(c))) //{a,c}
//    assert(netBefore.getModel.get == Set[Atom](a,c))
//
//
//    val netAfter = AnswerUpdateNetwork()
//    netAfter.add(Rule(a, c)) //{}
//    netAfter.add(Rule(c, a)) //{}
//    netAfter.add(Rule(b, none, Set(a))) //{b}
//    //
//    netAfter.add(Rule(n, Set(b), Set(c))) //None
//    assert(netAfter.getModel == None)
//    netAfter.add(Rule(a,none,none)) //{a,c}
//    assert(netAfter.getModel.get == Set[Atom](a,c))
//
//  }

  //constraints not implemented
//  test(":- not a") {
//    val net = AnswerUpdateNetwork()
//    net.add(Rule(n,none,Set(a)))
//    assert(net.getModel == None)
//  }

  //constraints not implemented
//  test("a. :- a.") {
//    val net = AnswerUpdateNetwork()
//    net.add(Rule(a))
//    net.add(Rule(n,a))
//    assert(net.getModel == None)
//  }

  //inconsistent: direct odd loop
//  test("a :- not a.") {
//    val net = AnswerUpdateNetwork()
//    net.add(Rule(a,Set(),Set(a)))
//    assert(net.getModel == None)
//  }

  //consistent: 'inactive' odd loop
  test("a. a :- not a.") {

    val netFactFirst = ExtendedJTMS()
    netFactFirst.add(AspRule(a))
    netFactFirst.add(AspRule(a,Set(),Set(a)))
    assert(netFactFirst.getModel.get == Set(a))

    val netRuleFirst = ExtendedJTMS()
    netRuleFirst.add(AspRule(a,Set(),Set(a)))
    netRuleFirst.add(AspRule(a))
    assert(netRuleFirst.getModel.get == Set(a))
  }

  //inconsistent: indirect odd loop
//  test("a :- b. b :- c. c :- not a.") {
//    val net = AnswerUpdateNetwork()
//    net.add(Rule(a, b))
//    net.add(Rule(b, c))
//    net.add(Rule(c, none, Set(a)))
//    assert(net.getModel == None)
//
//    net.add(Rule(c))
//    assert(net.getModel.get == Set[Atom](a, b, c))
//  }

  //inconsistent: indirect odd loop
//  test("a :- b. b :- c. c :- d. d :- not a.") {
//    val net = AnswerUpdateNetwork()
//    net.add(Rule(a, b))
//    net.add(Rule(b, c))
//    net.add(Rule(c, d))
//    net.add(Rule(d, none, Set(a)))
//    assert(net.getModel == None)
//
//    net.add(Rule(d))
//    assert(net.getModel.get == Set[Atom](a, b, c, d))
//  }

  //constraints not implemented
//  test("P5. a :- b.  b :- not c.  c :- not a.  n :- c.") {
//
//    val net1 = AnswerUpdateNetwork()
//    net1.add(Rule(a,b))
//    net1.add(Rule(b,none,Set(c)))
//    assert(net1.getModel.get == Set(a,b))
//
//    net1.add(Rule(c,none,Set(a)))
//    assert(net1.getModel.get == Set(a,b))
//
//    net1.add(Rule(n,c)) //:- c
//    //force other
//    assert(net1.getModel.get == Set(a,b))
//
//    //other insertion order of last two
//    val net2 = AnswerUpdateNetwork()
//    net2.add(Rule(a,b)) //a :- b
//    net2.add(Rule(c,none,Set(a))) //c :- not a
//    assert(net2.getModel.get == Set(c)) //{c}
//
//    net2.add(Rule(b,none,Set(c))) // b :- not c
//    assert(net2.getModel.get == Set(c)) //{c} (or {a,b})
//
//    net2.add(Rule(n,c)) //:- c
//    //force other
//    assert(net2.getModel.get == Set(a,b))
//
//  }

  //constraints not implemented
//  test("P6. a :- b.  b :- not c.  c :- not a.  n :- a.") {
//
//    val net1 = AnswerUpdateNetwork()
//    net1.add(Rule(a,b))
//    assert(net1.getModel.get == Set())
//
//    net1.add(Rule(b,none,Set(c)))
//    assert(net1.getModel.get == Set(a,b))
//
//    net1.add(Rule(c,none,Set(a)))
//    assert(net1.getModel.get == Set(a,b))
//
//    net1.add(Rule(n,a)) //:- a
//    //force other
//    assert(net1.getModel.get == Set(c))
//
//    //other insertion order
//    val net2 = AnswerUpdateNetwork()
//    net2.add(Rule(a,b)) //a :- b
//    net2.add(Rule(b,none,Set(c))) // b :- not c {a,b}
//    assert(net2.getModel.get == Set(a,b)) //{a,b}
//
//    net2.add(Rule(n,a)) //:- a
//    assert(net2.getModel == None)
//
//    net2.add(Rule(c,none,Set(a))) //c :- not a
//    assert(net2.getModel.get == Set(c)) //{c}
//
//  }

  //constraints not implemetned
//  test("elkan p228") {
//    val net = AnswerUpdateNetwork()
//    net.add(Rule(f,none,Set(a,c)))
//    net.add(Rule(b,none,Set(a)))
//    net.add(Rule(a,none,Set(b)))
//    assert(net.getModel.get == Set(b,f))
//
//    net.add(Rule(n,f))
//    assert(net.getModel.get == Set(a))
//  }

}
