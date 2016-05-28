package jtms.asp.examples

import core.Atom
import core.asp._
import fixtures.AtomTestFixture
import jtms.{ExtendedJtms, JtmsBeierle}
import org.scalatest.FunSuite

/**
  * Created by hb on 2016-04-28
  */
class AspAddRemove extends FunSuite with AtomTestFixture {

  val none = Set[Atom]()

  test("a :- b. b :- not c. c :- not d.") {
    
    val r1 = AspRule(a,b)
    val r2 = AspRule(b,none,Set(c))
    val r3 = AspRule(c,none,Set(d))
        
    val tms = ExtendedJtms()
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

  test("remove case repercussion vs cons-star") {
    val tms = ExtendedJtms()
    def m = tms.getModel.get

    tms.add(AspRule(a,b))
    tms.add(AspRule(b,a))
    tms.add(AspRule(b,Set(c))) //,Set(d)))
    tms.add(AspRule(c,none,Set(e)))
    tms.add(AspFact(e))
    /*
    the problem is that after this step, the support of a is empty, but it should be b.
    if supp(a)=b, then we can compute the correct solution with repercussions, which is desired.

    obs: it should not be the case that the support of an atom, that is not (only) a fact, has an empty
    support.
     */
    tms.remove(AspFact(e))
    assert(m == Set(a,b,c))
  }

  test("a :- b. b :- a. b :- c, not d. c :- not e. e.") {

    val tms = ExtendedJtms()
    def m = tms.getModel.get

    tms.add(AspRule(a,b))
    assert(m == Set())

    tms.add(AspRule(b,a))
    assert(m == Set())

    tms.add(AspRule(b,Set(c),Set(d)))
    assert(m == Set())

    tms.add(AspRule(c,none,Set(e)))
    assert(m == Set(a,b,c))

    tms.add(AspFact(e))
    assert(m == Set(e))

    tms.remove(AspFact(e))
    assert(m == Set(a,b,c)) //

    tms.add(AspFact(e))
    tms.remove(AspRule(b,Set(c),Set(d)))
    assert(m == Set(e))

    tms.remove(AspFact(e))
    assert(m == Set(c))

    tms.remove(AspRule(a,b))
    assert(m == Set(c))

    tms.add(AspRule(b,Set(c),Set(d)))
    assert(m == Set(b,c))

    tms.add(AspRule(a,b))
    assert(m == Set(a,b,c))

    tms.add(AspFact(e))
    assert(m == Set(e))

  }

  test("a :- x, not b. b :- y, not a. x :- not y. y :- not x.") {

    val a__x_not_b = AspRule(a,Set(x),Set(b))
    val b__y_not_a = AspRule(b,Set(y),Set(a))
    val x__not_y = AspRule(x,none,Set(y))
    val y__not_x = AspRule(y,none,Set(x))

    val tms = ExtendedJtms()
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

    val a__not_b = AspRule(a,none,Set(b))
    val b__c_not_a = AspRule(b,Set(c),Set(a))
    val x__a_c = AspRule(x,Set(a,c))
    val y__a_not_c = AspRule(y,Set(a),Set(c))
    val c__not_d = AspRule(c,none,Set(d))
    val d__not_c = AspRule(d,none,Set(c))

    val tms = ExtendedJtms()
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

  test("mess") {

    val tms = ExtendedJtms()
    def m = tms.getModel.get

    tms.add(AspRule(a,Set(b,c),Set(d,e))) //1
    tms.add(AspRule(b,Set(f,g),Set(h,i))) //2
    tms.add(AspRule(c,Set(j,k))) //3
    tms.add(AspRule(c,none,Set(d,e))) //4
    assert(m == Set(c))

    tms.add(AspRule(d,Set(k,l),Set(n))) //5
    tms.add(AspRule(e,none,Set(b,c))) //6
    assert(m == Set(c))

    tms.remove(AspRule(c,none,Set(d,e))) //7
    assert(m == Set(e))

    tms.add(AspRule(f,Set(e),Set(h))) //8
    assert(m == Set(e,f))

    tms.add(AspRule(g,Set(c),Set(i))) //9
    assert(m == Set(e,f))

    tms.add(AspRule(c,none,Set(d,e))) //10
    assert(m == Set(e,f))

    tms.remove(AspRule(e,none,Set(b,c))) //11
    assert(m == Set(c,g))

    tms.add(AspRule(h,Set(c,g),Set(f))) //12
    assert(m == Set(c,g,h))

    tms.add(AspRule(a,Set(h),Set(e))) //13
    assert(m == Set(a,c,g,h))

    tms.add(AspRule(e,none,Set(b,c))) //14
    assert(m == Set(a,c,g,h))

    tms.remove(AspRule(c,none,Set(d,e))) //15
    assert(m == Set(e,f))

    tms.add(AspRule(i,Set(e,f),Set(a))) //16
    assert(m == Set(e,f,i))

    tms.add(AspRule(a,Set(e,f),Set(i))) //17
    assert(m == Set(e,f,i))

    tms.remove(AspRule(i,Set(e,f),Set(a))) //18
    assert(m == Set(e,f,a))

    tms.add(AspRule(c,none,Set(d,e))) //19
    assert(m == Set(e,f,a))

    tms.remove(AspRule(e,none,Set(b,c))) //20
    assert(m == Set(c,g,h,a))

    tms.add(AspRule(j,Set(c,g),Set(a))) // 21
    assert(m == Set(c,g,h,a))

    tms.remove(AspRule(a,Set(e,f),Set(i))) //22
    assert(m == Set(a,c,g,h))

    tms.add(AspRule(b,none,Set(e))) //23
    assert(m == Set(a,b,c,g,h))

    tms.add(AspRule(e,none,Set(b,c))) //24
    assert(m == Set(a,b,c,g,h))

    tms.remove(AspRule(c,none,Set(d,e))) //25
    assert(m == Set(b) || m == Set(e,f)) //!

    tms.remove(AspRule(b,none,Set(e))) //26
    assert(m == Set(e,f))

  }

  val g = Atom("g")
  val h = Atom("h")
  val i = Atom("i")
  val j = Atom("j")
  val k = Atom("k")
  val l = Atom("l")
  val n = Atom("n")

  test("reach") {

    /* edge(a,b). edge(b,c). edge(c,d). edge(d,e). edge(b,e).
       reach(X,Y) :- edge(X,Y), not blocked(X,Y).
       reach(X,Y) :- reach(X,Z), edge(Z,Y), not blocked(Z,Y).
    */

    val tms = ExtendedJtms()

    val a = "a"
    val b = "b"
    val c = "c"
    val d = "d"
    val e = "e"
    def edge(x:String, y:String) = Atom("edge("+x+","+y+")")
    def reach(x:String, y:String) = Atom("reach("+x+","+y+")")
    def blocked(x:String, y:String) = Atom("blocked("+x+","+y+")")

    tms.add(edge(a,b))
    tms.add(edge(b,c))
    tms.add(edge(c,d))
    tms.add(edge(d,e))
    tms.add(edge(b,e))

    val C = List(a,b,c,d,e)
    //reach(X,Y) :- edge(X,Y), not blocked(X,Y).
    for (x <- C) {
      for (y <- C) {
        val r = AspRule(reach(x,y),Set(edge(x,y)),Set(blocked(x,y)))
        tms.add(r)
        //println(r)
      }
    }
    //reach(X,Y) :- reach(X,Z), edge(Z,Y), not blocked(Z,Y).
    for (x <- C) {
      for (y <- C) {
        for (z <- C) {
          val r = AspRule(reach(x,y),Set(reach(x,z),edge(z,y)),Set(blocked(z,y)))
          tms.add(r)
          //println(r)
        }
      }
    }

    def m = tms.getModel.get

    assert(m contains reach(a,b))
    assert(m contains reach(b,c))
    assert(m contains reach(c,d))
    assert(m contains reach(d,e))
    assert(m contains reach(a,c))
    assert(m contains reach(a,d))
    assert(m contains reach(a,e))
    assert(m contains reach(b,c))
    assert(m contains reach(b,d))
    assert(m contains reach(b,e))
    assert(m contains reach(c,d))
    assert(m contains reach(c,e))
    assert(m contains reach(d,e))

    tms.add(blocked(b,c))
    assert(!(m contains reach(b,d)))
    assert(m contains reach(b,e))
    assert(m contains reach(c,d))
    assert(m contains reach(c,e))
    assert(m contains reach(a,e))

    tms.add(blocked(b,e))
    assert(!(m contains reach(b,e)))
    assert(m contains reach(d,e))
    assert(m contains reach(c,e))
    assert(!(m contains reach(a,e)))

    tms.remove(blocked(b,c))
    assert(m contains reach(b,c))
    assert(m contains reach(b,d))
    assert(m contains reach(b,e))
    assert(m contains reach(a,e))

  }

  test("jtms5") {

    /*
       breaks with normal doyle implementation, even if fixOut uses options.
     */

    val r1 = AspRule(a, c)
    val r2 = AspRule(b, none, Set(a))
    val r3 = AspRule(c, a)
    val r4a = AspRule(d, b)
    val r4b = AspRule(d, c)
    val r5 = AspFact(e)
    val r6 = AspRule(f, Set(c, e))
    val r0 = AspFact(a)

    val tms = ExtendedJtms(AspProgram(r1,r2,r3,r4a,r4b,r5,r6))
    def m = tms.getModel.get

    assert(m == Set(e,b,d))

    tms.add(r0)
    assert(m == Set(a,c,d,e,f))

    tms.remove(r0)
    assert(m == Set(e,b,d))

  }

  test("jtms5 essence") {

    //works with doyle if c and b are replaced (i.e., a :- b. b :- a. etc.)

    val tms = ExtendedJtms(AspProgram(
      AspRule(a,c), //a :- c
      AspRule(c,a), //c :- a
      AspRule(b,none,Set(a)), //b :- not a
      AspRule(d,b), //d :- b
      AspRule(d,c)) //d :- c
    )

    def m = tms.getModel.get

    assert(m == Set(b,d))

    tms.add(AspFact(a))
    assert(m == Set(a,c,d))

    tms.remove(AspFact(a))
    assert(m == Set(b,d))
  }

  test("jtms5 essence part") {
    val tms = ExtendedJtms(AspProgram(
      AspRule(a,c), //a :- c
      AspRule(c,a), //c :- a
      AspRule(b,none,Set(a)), //b :- not a
      AspRule(d,b))) //d :- b)

    def m = tms.getModel.get

    tms.add(AspFact(a))
    assert(m == Set(a,c))

    tms.remove(AspFact(a))
    assert(m == Set(b,d))
  }

  test("jtms5-like problem for add") {

    val tms = ExtendedJtms(AspProgram(
      AspRule(a,c), //a :- c
      AspRule(c,a), //c :- a
      AspRule(b,none,Set(a)), //b :- not a
      AspRule(d,b), //d :- b
      AspRule(d,c)) //d :- c
    )

    def m = tms.getModel.get

    assert(m == Set(b,d))

    tms.add(AspRule(a,none,Set(e))) //instead of AspFact(a)
    assert(m == Set(a,c,d))

    tms.add(AspFact(e)) //instead of removing fact a directly
    assert(m == Set(e,b,d))
  }

  test("Beierle: jtms5-like problem for add") {

    val tms = JtmsBeierle(AspProgram(
      AspRule(a,c), //a :- c
      AspRule(c,a), //c :- a
      AspRule(b,none,Set(a)), //b :- not a
      AspRule(d,b), //d :- b
      AspRule(d,c)) //d :- c
    )

    def m = tms.getModel.get

    assert(m == Set(b,d))

    tms.add(AspRule(a,none,Set(e))) //instead of AspFact(a)
    assert(m == Set(a,c,d))

    //this one reveals a bug:
//    tms.add(AspFact(e)) //instead of removing fact a directly
//    assert(m == Set(e,b,d))
  }

  test("beierle tests") {
    val tms = JtmsBeierle(AspProgram(
      AspRule(a,c), //a :- c
      AspRule(c,a), //c :- a
      AspRule(b,none,Set(a)), //b :- not a
      AspRule(d,b), //d :- b
      AspRule(d,c))) //d :- c

    def m = tms.getModel.get

    tms.add(AspFact(a))
    assert(m == Set(a,c,d))
  }

}
