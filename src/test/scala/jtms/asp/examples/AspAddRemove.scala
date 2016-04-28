package jtms.asp.examples

import core.{Rule, Atom}
import jtms.ExtendedJTMS
import org.scalatest.FunSuite

/**
  * Created by hb on 2016-04-28
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

  test("a :- not b. b :- c, not a. x :- a, c. y :- a, not c. c :- not d. d :- not c.") {

    val a__not_b = Rule(a,none,Set(b))
    val b__c_not_a = Rule(b,Set(c),Set(a))
    val x__a_c = Rule(x,Set(a,c))
    val y__a_not_c = Rule(y,Set(a),Set(c))
    val c__not_d = Rule(c,none,Set(d))
    val d__not_c = Rule(d,none,Set(c))

    val tms = ExtendedJTMS()
    def m = tms.getModel.get

    tms.add(x__a_c)
    tms.add(y__a_not_c)
    tms.add(b__c_not_a)
    assert(m == Set())

    tms.add(d__not_c)
    assert(m == Set(d))

    tms.add(c__not_d)
    assert(m == Set(d))

    tms.remove(d__not_c)
    assert(m == Set(b,c))

    tms.remove(b__c_not_a)
    assert(m == Set(c))

    tms.add(a__not_b)
    assert(m == Set(a,c,x))

    tms.add(b__c_not_a)
    assert(m == Set(a,c,x))

    tms.add(d__not_c)
    assert(m == Set(a,c,x))

    tms.remove(c__not_d)
    assert(m == Set(a,d,y))

    tms.remove(a__not_b)
    assert(m == Set(d))

    tms.remove(d__not_c)
    assert(m == Set())

    tms.add(c__not_d)
    assert(m == Set(b,c))

    tms.add(a__not_b)
    assert(m == Set(b,c))

    tms.remove(b__c_not_a)
    assert(m == Set(a,c,x))

    tms.add(b__c_not_a)
    assert(m == Set(a,c,x))

    tms.add(d__not_c)
    assert(m == Set(a,c,x))

  }


}
