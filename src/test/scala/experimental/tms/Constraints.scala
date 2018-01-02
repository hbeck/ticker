package experimental.tms

import core.asp.{AspFact, AspRule}
import core.{Atom, ContradictionAtom, Predicate}
import fixtures.AtomTestFixture
import reasoner.incremental.jtms.algorithms.Jtms
import reasoner.incremental.jtms.networks.OptimizedNetwork
import org.scalatest.FunSuite

import scala.util.Random

/**
  * Created by hb on 12.03.16.
  */
class Constraints extends FunSuite with AtomTestFixture{

  val n = ContradictionAtom(Predicate("n"))

  val none = Set[Atom]()

  val times = 100

  test("P1: a :- not b.  b :- not a.  n :- a.") {
    for (i <- 1 to times) {
      val jtms = Jtms()
      jtms.add(AspRule(a, none, Set(b)))
      jtms.add(AspRule(b, none, Set(a))) //-> {a}

      var model = jtms.getModel.get
      assert(model == Set(a))

      jtms.add(AspRule(n, a))
      model = jtms.getModel.get
      assert(model == Set(b))
    }
  }

  test("P2: b :- not a.  n :- b, not c.") { //JTMS_21 base case
    for (i <- 1 to times) {
      val jtms0 = Jtms()

      jtms0.add(AspRule(n, Set(b), Set(c)))
      assert(jtms0.getModel.get == Set[Atom]())

      jtms0.add(AspRule(b, none, Set(a)))
      assert(jtms0.getModel.get == Set(a)) //diff to ASP, which has None

      val jtms1 = Jtms()

      jtms1.add(AspRule(b, none, Set(a)))
      assert(jtms1.getModel.get == Set(b))

      jtms1.add(AspRule(n, Set(b), Set(c)))
      assert(jtms1.getModel.get == Set(a)) //diff to ASP, which has None
    }
  }

  test("P3: a :- c.  c :- a.  b :- not a.  n :- b, not c.") {
    for (i <- 1 to times) {
      val jtms = Jtms()
      jtms.add(AspRule(a, c))

      assert(jtms.getModel.get.isEmpty)

      jtms.add(AspRule(c, a))
      assert(jtms.getModel.get.isEmpty)

      jtms.add(AspRule(b, none, Set(a)))
      assert(jtms.getModel.get.size == 1)
      assert(jtms.getModel.get contains b)

      jtms.add(AspRule(n, Set(b), Set(c)))
      assert(jtms.getModel.get == Set(a,c)) //diff to ASP, which has None
    }
  }

  test("P4:  a :- c.  c :- a.  b :- not a.  n :- b, not c.  a.") {
    for (i <- 1 to times) {
      val jtmsBefore = Jtms()
      jtmsBefore.add(AspRule(a, c)) //{}
      jtmsBefore.add(AspRule(c, a)) //{}
      jtmsBefore.add(AspRule(b, none, Set(a))) //{b}
      //
      jtmsBefore.add(AspRule(a,none,none)) //{a,c}
      assert(jtmsBefore.getModel.get == Set[Atom](a,c))
      jtmsBefore.add(AspRule(n, Set(b), Set(c))) //{a,c}
      assert(jtmsBefore.getModel.get == Set[Atom](a,c))


      val jtmsAfter = Jtms()
      jtmsAfter.add(AspRule(a, c)) //{}
      jtmsAfter.add(AspRule(c, a)) //{}
      jtmsAfter.add(AspRule(b, none, Set(a))) //{b}
      //
      jtmsAfter.add(AspRule(n, Set(b), Set(c))) //None
      assert(jtmsAfter.getModel.get == Set(a,c)) //diff to ASP, which has none
      jtmsAfter.add(AspRule(a,none,none)) //{a,c}
      assert(jtmsAfter.getModel.get == Set[Atom](a,c))

    }
  }

  //inconsistent
  test(":- not a") {
    val jtms = Jtms()
    jtms.add(AspRule(n,none,Set(a)))
    assert(jtms.getModel == None)
  }

  //inconsistent
  test("a. :- a.") {
    val jtms = Jtms()
    jtms.add(AspFact(a))
    jtms.add(AspRule(n,a))
    assert(jtms.getModel == None)
  }

  //consistent: 'inactive' odd loop
  test("a. a :- not a.") {

    val jtmsFactFirst = Jtms()
    jtmsFactFirst.add(AspFact(a))
    jtmsFactFirst.add(AspRule(a,Set(),Set(a)))
    assert(jtmsFactFirst.getModel.get == Set(a))

    val jtmsRuleFirst = Jtms()
    jtmsRuleFirst.add(AspRule(a,Set(),Set(a)))
    jtmsRuleFirst.add(AspFact(a))
    assert(jtmsRuleFirst.getModel.get == Set(a))
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

  test("P5. a :- b.  b :- not c.  c :- not a.  n :- c.") {

    val jtms1 = Jtms()
    jtms1.add(AspRule(a,b))
    jtms1.add(AspRule(b,none,Set(c)))
    assert(jtms1.getModel.get == Set(a,b))

    jtms1.add(AspRule(c,none,Set(a)))
    assert(jtms1.getModel.get == Set(a,b))

    jtms1.add(AspRule(n,c)) //:- c
    //force other
    assert(jtms1.getModel.get == Set(a,b))

    //other insertion order of last two
    val jtms2 = Jtms()
    jtms2.add(AspRule(a,b)) //a :- b
    jtms2.add(AspRule(c,none,Set(a))) //c :- not a
    assert(jtms2.getModel.get == Set(c)) //{c}

    jtms2.add(AspRule(b,none,Set(c))) // b :- not c
    assert(jtms2.getModel.get == Set(c)) //{c} (or {a,b})

    jtms2.add(AspRule(n,c)) //:- c
    //force other
    assert(jtms2.getModel.get == Set(a,b))

  }

  test("P6. a :- b.  b :- not c.  c :- not a.  n :- a.") {

    val net1 = new OptimizedNetwork()
    val jtms1 = Jtms(net1,new Random())

    jtms1.add(AspRule(a,b)) //a :- b
    assert(jtms1.getModel.get == Set())

    jtms1.add(AspRule(b,none,Set(c))) //b :- not c
    assert(jtms1.getModel.get == Set(a,b))

    jtms1.add(AspRule(c,none,Set(a))) //c :- not a
    assert(jtms1.getModel.get == Set(a,b))

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

    jtms1.add(AspRule(n,Set(a,b),none)) //:- a,b
    //force other
    assert(jtms1.getModel.get == Set(c))

    //TODO
    //other insertion order
    val net2 = new OptimizedNetwork()
    val update2 = Jtms(net2, new Random())

    update2.add(AspRule(a,b)) //a :- b
    update2.add(AspRule(b,none,Set(c))) // b :- not c  => {a,b}
    assert(update2.getModel.get == Set(a,b)) //{a,b}

    update2.add(AspRule(n,Set(a,b),none)) //:- a,b
    assert(update2.getModel == None)

    update2.add(AspRule(c,none,Set(a))) //c :- not a
    assert(update2.getModel.get == Set(c)) //{c}

  }

  val t1 = Atom("t1")
  val t2 = Atom("t2")
  val r1 = Atom("r1")
  val r2 = Atom("r2")

  test("doyle time room") {
    val jtms = Jtms()
    jtms.add(AspRule(t1,none,Set(t2)))
    jtms.add(AspRule(r1,none,Set(r2)))
    assert(jtms.getModel.get == Set(t1,r1))

    jtms.add(AspRule(n,Set(t1,r1),none))
    assert(jtms.getModel.get == Set(t1,r2)) //not an answer set!
  }

  test("elkan p228") {
    val jtms = Jtms()
    jtms.add(AspRule(f, none, Set(a, c)))
    jtms.add(AspRule(b, none, Set(a)))
    jtms.add(AspRule(a, none, Set(b)))
    assert(jtms.getModel.get == Set(b, f))

    jtms.add(AspRule(n, f))
    assert(jtms.getModel.get == Set(a))
  }

}
