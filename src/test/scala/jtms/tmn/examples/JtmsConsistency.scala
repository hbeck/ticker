package jtms.tmn.examples

import core.asp.{AspFact, AspRule}
import core.{Atom, ContradictionAtom, Predicate}
import jtms._
import jtms.algorithms.JtmsDoyle
import jtms.networks.OptimizedNetwork
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class JtmsConsistency extends FunSuite {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")
//  val e = Atom("e")
  val f = Atom("f")
  val t1 = Atom("t1")
  val t2 = Atom("t2")
  val r1 = Atom("r1")
  val r2 = Atom("r2")
  val x = Atom("x")

  val n = ContradictionAtom(Predicate("n"))

  val none = Set[Atom]()

  val times = 100

  test("a") {
    val tmn = JtmsDoyle() //TODO
    var model = tmn.getModel.get
    assert(model.isEmpty)

    tmn.add(AspFact(a))
    model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains a)
  }

  test("a :- not b. then b.") {

    val tmn = JtmsDoyle()
    tmn.add(AspRule(a,none,Set(b)))
    var model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains a)

    tmn.add(AspRule(b,none,none))
    model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains b)

  }

  test("a :- not b. b :- not a.  b.") {

    val tmn = JtmsDoyle()
    tmn.add(AspRule(a,none,Set(b)))
    tmn.add(AspRule(b,none,Set(a)))
    var model = tmn.getModel.get
    assert(model == Set(a))

    tmn.add(AspRule(b,none,none))
    model = tmn.getModel.get
    assert(model == Set(b))

  }

  test("P1: a :- not b.  b :- not a.  n :- a.") {
    for (i <- 1 to times) {
      val tmn = JtmsDoyle()
      tmn.add(AspRule(a, none, Set(b)))
      tmn.add(AspRule(b, none, Set(a))) //-> {a}

      var model = tmn.getModel.get
      assert(model == Set(a))

      tmn.add(AspRule(n, a))
      model = tmn.getModel.get
      assert(model == Set(b))
    }
  }

  test("P2: b :- not a.  n :- b, not c.") { //JTMS_21 base case
    for (i <- 1 to times) {
      val tmn0 = JtmsDoyle()

      tmn0.add(AspRule(n, Set(b), Set(c)))
      assert(tmn0.getModel.get == Set[Atom]())

      tmn0.add(AspRule(b, none, Set(a)))
      assert(tmn0.getModel.get == Set(a)) //diff to ASP, which has None

      val tmn1 = JtmsDoyle()

      tmn1.add(AspRule(b, none, Set(a)))
      assert(tmn1.getModel.get == Set(b))

      tmn1.add(AspRule(n, Set(b), Set(c)))
      assert(tmn1.getModel.get == Set(a)) //diff to ASP, which has None
    }
  }

  test("P3: a :- c.  c :- a.  b :- not a.  n :- b, not c.") {
    for (i <- 1 to times) {
      val tmn = JtmsDoyle()
      tmn.add(AspRule(a, c))

      assert(tmn.getModel.get.isEmpty)

      tmn.add(AspRule(c, a))
      assert(tmn.getModel.get.isEmpty)

      tmn.add(AspRule(b, none, Set(a)))
      assert(tmn.getModel.get.size == 1)
      assert(tmn.getModel.get contains b)

      tmn.add(AspRule(n, Set(b), Set(c)))
      assert(tmn.getModel.get == Set(a,c)) //diff to ASP, which has None
    }
  }

  test("P4:  a :- c.  c :- a.  b :- not a.  n :- b, not c.  a.") {
    for (i <- 1 to times) {
      val tmnBefore = JtmsDoyle()
      tmnBefore.add(AspRule(a, c)) //{}
      tmnBefore.add(AspRule(c, a)) //{}
      tmnBefore.add(AspRule(b, none, Set(a))) //{b}
      //
      tmnBefore.add(AspRule(a,none,none)) //{a,c}
      assert(tmnBefore.getModel.get == Set[Atom](a,c))
      tmnBefore.add(AspRule(n, Set(b), Set(c))) //{a,c}
      assert(tmnBefore.getModel.get == Set[Atom](a,c))


      val tmnAfter = JtmsDoyle()
      tmnAfter.add(AspRule(a, c)) //{}
      tmnAfter.add(AspRule(c, a)) //{}
      tmnAfter.add(AspRule(b, none, Set(a))) //{b}
      //
      tmnAfter.add(AspRule(n, Set(b), Set(c))) //None
      assert(tmnAfter.getModel.get == Set(a,c)) //diff to ASP, which has none
      tmnAfter.add(AspRule(a,none,none)) //{a,c}
      assert(tmnAfter.getModel.get == Set[Atom](a,c))

    }
  }

  //inconsistent
  test(":- not a") {
    val tmn = JtmsDoyle()
    tmn.add(AspRule(n,none,Set(a)))
    assert(tmn.getModel == None)
  }

  //inconsistent
  test("a. :- a.") {
    val tmn = JtmsDoyle()
    tmn.add(AspFact(a))
    tmn.add(AspRule(n,a))
    assert(tmn.getModel == None)
  }

  //consistent: 'inactive' odd loop
  test("a. a :- not a.") {

    val tmnFactFirst = JtmsDoyle()
    tmnFactFirst.add(AspFact(a))
    tmnFactFirst.add(AspRule(a,Set(),Set(a)))
    assert(tmnFactFirst.getModel.get == Set(a))

    val tmnRuleFirst = JtmsDoyle()
    tmnRuleFirst.add(AspRule(a,Set(),Set(a)))
    tmnRuleFirst.add(AspFact(a))
    assert(tmnRuleFirst.getModel.get == Set(a))
  }

  //inconsistent: direct odd loop
//  test("a :- not a.") {
//
//    val tmn = JtmsRefactored()
//    tmn.add(Rule(a,Set(),Set(a)))
//    assert(tmn.getModel == None)
//
//  }

  //inconsistent: indirect odd loop
//  test("a :- b. b :- c. c :- not a.") {
//
//    for (i <- 1 to times) {
//      val tmn = TMN()
//      tmn.add(Rule(a, b))
//      tmn.add(Rule(b, c))
//      tmn.add(Rule(c, none, Set(a)))
//      assert(tmn.getModel == None)
//
//      tmn.add(Rule(c))
//      assert(tmn.getModel.get == Set[Atom](a, b, c))
//    }
//  }

  //inconsistent: indirect odd loop
//  test("a :- b. b :- c. c :- d. d :- not a.") {
//
//    for (i <- 1 to times) {
//      val tmn = TMN()
//      tmn.add(Rule(a, b))
//      tmn.add(Rule(b, c))
//      tmn.add(Rule(c, d))
//      tmn.add(Rule(d, none, Set(a)))
//      assert(tmn.getModel == None)
//
//      tmn.add(Rule(d))
//      assert(tmn.getModel.get == Set[Atom](a, b, c, d))
//    }
//  }

  test("even loop. a :- b not. b :- not c. c :- not d. d :- not a.") { //{a,c} or {b,d}

    for (i <- 1 to times) {
      val tmn = JtmsDoyle()
      tmn.add(AspRule(a, none, Set(b)))
      tmn.add(AspRule(b, none, Set(c)))
      tmn.add(AspRule(c, none, Set(d)))
      tmn.add(AspRule(d, none, Set(c)))
      assert(tmn.getModel.get == Set(a,c))

      //tmn.add(Rule(n,Set(a)))
      //assert(tmn.getModel.get == Set[Atom](b,c)) //ASP would be {b,d} !!
    }
  }


  test("P5. a :- b.  b :- not c.  c :- not a.  n :- c.") {

    val tmn1 = JtmsDoyle()
    tmn1.add(AspRule(a,b))
    tmn1.add(AspRule(b,none,Set(c)))
    assert(tmn1.getModel.get == Set(a,b))

    tmn1.add(AspRule(c,none,Set(a)))
    assert(tmn1.getModel.get == Set(a,b))

    tmn1.add(AspRule(n,c)) //:- c
    //force other
    assert(tmn1.getModel.get == Set(a,b))

    //other insertion order of last two
    val tmn2 = JtmsDoyle()
    tmn2.add(AspRule(a,b)) //a :- b
    tmn2.add(AspRule(c,none,Set(a))) //c :- not a
    assert(tmn2.getModel.get == Set(c)) //{c}

    tmn2.add(AspRule(b,none,Set(c))) // b :- not c
    assert(tmn2.getModel.get == Set(c)) //{c} (or {a,b})

    tmn2.add(AspRule(n,c)) //:- c
    //force other
    assert(tmn2.getModel.get == Set(a,b))

  }

  test("P6. a :- b.  b :- not c.  c :- not a.  n :- a.") {

    val net1 = new OptimizedNetwork()
    val update = new JtmsDoyle(net1)

    update.add(AspRule(a,b)) //a :- b
    assert(update.getModel.get == Set())

    update.add(AspRule(b,none,Set(c))) //b :- not c
    assert(update.getModel.get == Set(a,b))

    update.add(AspRule(c,none,Set(a))) //c :- not a
    assert(update.getModel.get == Set(a,b))

    assert(net1.supp(a)==Set(b))
    assert(net1.supp(b)==Set(c))
    assert(net1.supp(c)==Set(a))
    Set(a,b,c) foreach ( x => assert(net1.ancestors(x) == Set(a,b,c)) )

    assert(net1.antecedents(a)==Set(b))
    assert(net1.antecedents(b)==Set(c))
    assert(net1.antecedents(c)==Set())
    //
    assert(net1.foundations(a)==Set(b,c))
    assert(net1.foundations(b)==Set(c))
    assert(net1.foundations(c)==Set())

    assert(net1.cons(a)==Set(c))
    assert(net1.cons(b)==Set(a))
    assert(net1.cons(c)==Set(b))
    //
    assert(net1.affected(a)==Set(c))
    assert(net1.affected(b)==Set(a))
    assert(net1.affected(c)==Set(b))
    //
    assert(net1.repercussions(a)==Set(a,b,c))
    assert(net1.repercussions(b)==Set(a,b,c))
    assert(net1.repercussions(c)==Set(a,b,c))

//    net1.add(Rule(a,d))
//    assert(net1.cons(d)==Set(a))
//    assert(net1.aff(d)==Set())
//    assert(net1.repercussions(d)==Set())

//    net1.add(Rule(x,Set(a,b)))
//    assert(net1.foundations(x) == Set(a,b,c))
//    assert((Set(a,b,c) filter (net1.isAssumption(_))) == Set(b))

    update.add(AspRule(n,Set(a,b))) //:- a,b
    //force other
    assert(update.getModel.get == Set(c))

    //TODO
    //other insertion order
    val net2 = new OptimizedNetwork()
    val update2 = new JtmsDoyle(net2)

    update2.add(AspRule(a,b)) //a :- b
    update2.add(AspRule(b,none,Set(c))) // b :- not c  => {a,b}
    assert(update2.getModel.get == Set(a,b)) //{a,b}

    update2.add(AspRule(n,Set(a,b))) //:- a,b
    assert(update2.getModel == None)

    update2.add(AspRule(c,none,Set(a))) //c :- not a
    assert(update2.getModel.get == Set(c)) //{c}

  }

  test("doyle time room") {
    val tmn = JtmsDoyle()
    tmn.add(AspRule(t1,none,Set(t2)))
    tmn.add(AspRule(r1,none,Set(r2)))
    assert(tmn.getModel.get == Set(t1,r1))

    tmn.add(AspRule(n,Set(t1,r1)))
    assert(tmn.getModel.get == Set(t1,r2)) //not an answer set!
  }

  test("elkan p228") {
    val tmn = JtmsDoyle()
    tmn.add(AspRule(f,none,Set(a,c)))
    tmn.add(AspRule(b,none,Set(a)))
    tmn.add(AspRule(a,none,Set(b)))
    assert(tmn.getModel.get == Set(b,f))

    tmn.add(AspRule(n,f))
    assert(tmn.getModel.get == Set(a))
  }

}
