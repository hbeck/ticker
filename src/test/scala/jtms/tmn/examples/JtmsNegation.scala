package jtms.tmn.examples

import core.Atom
import core.asp.{AspFact, AspRule}
import jtms._
import jtms.algorithms.Jtms
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class JtmsNegation extends FunSuite {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")
//  val e = Atom("e")
  val f = Atom("f")

  val none = Set[Atom]()

  val times = 100

  test("a") {
    val tmn = Jtms()
    var model = tmn.getModel.get
    assert(model.isEmpty)

    tmn.add(AspFact(a))
    model = tmn.getModel.get
    assert(model.size == 1)
    assert(model contains a)
  }

  test("a :- not b. then b.") {

    val jtms = Jtms()
    jtms.add(AspRule(a,none,Set(b)))
    var model = jtms.getModel.get
    assert(model.size == 1)
    assert(model contains a)

    jtms.add(AspRule(b,none,none))
    model = jtms.getModel.get
    assert(model.size == 1)
    assert(model contains b)

  }

  test("a :- not b. b :- not a.  b.") {

    val jtms = Jtms()
    jtms.add(AspRule(a,none,Set(b)))
    jtms.add(AspRule(b,none,Set(a)))
    var model = jtms.getModel.get
    assert(model == Set(a))

    jtms.add(AspRule(b,none,none))
    model = jtms.getModel.get
    assert(model == Set(b))

  }

  test("even loop. a :- b not. b :- not c. c :- not d. d :- not a.") { //{a,c} or {b,d}

    for (i <- 1 to times) {
      val jtms = Jtms()
      jtms.add(AspRule(a, none, Set(b)))
      jtms.add(AspRule(b, none, Set(c)))
      jtms.add(AspRule(c, none, Set(d)))
      jtms.add(AspRule(d, none, Set(c)))
      assert(jtms.getModel.get == Set(a,c))

    }
  }

}
