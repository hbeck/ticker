package jtms.asp.examples

import core.{Rule, Atom}
import jtms.ExtendedJTMS
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class AspAddRemove extends FunSuite {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")
  val e = Atom("e")
  val f = Atom("f")
  val x = Atom("x")
  val y = Atom("y")
  val z = Atom("z")

  val none = Set[Atom]()

  test("a :- b. b :- not c. c :- not d.") {
    
    val r1 = Rule(a,b)
    val r2 = Rule(b,none,Set(c))
    val r3 = Rule(c,none,Set(d))    
        
    val tms = ExtendedJTMS()
    def m = tms.getModel.get
    
    tms.add(r1)
    assert(m == Set())
    
    tms.add(r2)
    assert(m == Set(a,b))
    
    tms.remove(r2)
    assert(m == Set())
    
    tms.remove(r1)
    assert(m == Set())

    tms.add(r2)
    assert(m == Set(b))

    tms.add(r1)
    assert(m == Set(a,b))

    tms.remove(r1)
    assert(m == Set(b))

    tms.add(r1)
    assert(m == Set(a,b))

    tms.add(r3)
    assert(m == Set(c))

    tms.remove(r3)
    assert(m == Set(a,b))

    tms.add(r3)
    assert(m == Set(c))

    tms.remove(r1)
    assert(m == Set(c))

    tms.remove(r2)
    assert(m == Set(c))

    tms.add(r2)
    tms.remove(r3)
    assert(m == Set(b))
  }

  test("a :- b. b :- a. b :- c, not d. c :- not e. e.") {

    val tms = ExtendedJTMS()
    def m = tms.getModel.get

    tms.add(Rule(a,b))
    assert(m == Set())

    tms.add(Rule(b,a))
    assert(m == Set())

    tms.add(Rule(b,Set(c),Set(d)))
    assert(m == Set())

    tms.add(Rule(c,none,Set(e)))
    assert(m == Set(a,b,c))

    tms.add(Rule(e))
    assert(m == Set(e))

    tms.remove(Rule(e))
    assert(m == Set(a,b,c))

    tms.add(Rule(e))
    tms.remove(Rule(b,Set(c),Set(d)))
    assert(m == Set(e))

    tms.remove(Rule(e))
    assert(m == Set(c))

    tms.remove(Rule(a,b))
    assert(m == Set(c))

    tms.add(Rule(b,Set(c),Set(d)))
    assert(m == Set(b,c))

    tms.add(Rule(a,b))
    assert(m == Set(a,b,c))

    tms.add(Rule(e))
    assert(m == Set(e))

  }

  test("a :- x, not b. b :- y, not a. x :- not y. y :- not x.") {

    val a__x_not_b = Rule(a,Set(x),Set(b))
    val b__y_not_a = Rule(b,Set(y),Set(a))
    val x__not_y = Rule(x,none,Set(y))
    val y__not_x = Rule(y,none,Set(x))

    val tms = ExtendedJTMS()
    def m = tms.getModel.get

    tms.add(a__x_not_b)
    assert(m == Set())

    tms.add(b__y_not_a)
    assert(m == Set())

    tms.add(x__not_y)
    assert(m == Set(a,x))

    tms.remove(x__not_y)
    assert(m == Set())

    tms.add(y__not_x)
    assert(m == Set(b,y))

    tms.add(x__not_y)
    assert(m == Set(b,y))

    tms.remove(y__not_x)
    assert(m == Set(a,x))

    tms.remove(a__x_not_b)
    assert(m == Set(x))

    tms.add(y__not_x)
    assert(m == Set(x))

    tms.remove(x__not_y)
    assert(m == Set(b,y))

    tms.remove(b__y_not_a)
    assert(m == Set(y))

  }


}
