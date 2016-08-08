package jtms.asp.examples

import core.Atom
import core.asp._
import fixtures.AtomTestFixture
import jtms._
import org.scalatest.FunSuite

/**
  * Created by hb on 2016-07-31
  */
class JtmsLearnTests extends FunSuite with AtomTestFixture {
  
  //def jtmsImpl = JtmsDoyle
  def jtmsImpl = JtmsGreedy  

  val none = Set[Atom]()

  val times = 1 to 1000

  val n = Atom("n")

  test("jtms5") {

    var failures = 0

    val r1 = AspRule(a, c)
    val r2 = AspRule(b, none, Set(a))
    val r3 = AspRule(c, a)
    val r4a = AspRule(d, b)
    val r4b = AspRule(d, c)
    val r5 = AspFact(e)
    val r6 = AspRule(f, Set(c, e))
    val r0 = AspFact(a)

    val tms = JtmsLearn(AspProgram(r1, r2, r3, r4a, r4b, r5, r6))
    def m = tms.getModel.get

    assert(m == Set(e, b, d))

    tms.add(r0)
    assert(m == Set(a, c, d, e, f))

    times foreach { _ =>

      tms.remove(r0)

      if (failsToCompute(tms, Set(e,b,d))) failures += 1

      tms.add(r0)

    }

    println("failures: "+failures)
    printAvoidanceMap(tms)

  }

  def printAvoidanceMap(tms: Jtms): Unit = {
    if (!tms.isInstanceOf[JtmsLearn]) return
    val jtms = tms.asInstanceOf[JtmsLearn]
    println("learned avoidance map:")
    for ((k,v) <- jtms.avoidanceMap) {
      println(k+" -> "+v)
    }
  }

  //returns true if failure
  def failsToCompute(tms: Jtms, model: Set[Atom]): Boolean = {
    if (tms.getModel == None) {
      if (tms.isInstanceOf[JtmsLearn]) {
        val jtms = tms.asInstanceOf[JtmsLearn]
        println(jtms.state)
        println("sel. atom: "+jtms.selectedAtom+"\n")
      }
      return true
    } else {
      assert(tms.getModel.get == model)
      return false
    }
  }

  def failsToCompute(tms: Jtms, condition: => Boolean): Boolean = {
    if (tms.getModel == None) {
      if (tms.isInstanceOf[JtmsLearn]) {
        val jtms = tms.asInstanceOf[JtmsLearn]
        println(jtms.state)
        println("sel. atom: "+jtms.selectedAtom+"\n")
      }
      return true
    } else {
      assert(condition)
      return false
    }
  }

  test("jtms5 essence") {

    //works with doyle if c and b are replaced (i.e., a :- b. b :- a. etc.)

    val tms = JtmsLearn(AspProgram(
      AspRule(a, c), //a :- c
      AspRule(c, a), //c :- a
      AspRule(b, none, Set(a)), //b :- not a
      AspRule(d, b), //d :- b
      AspRule(d, c)) //d :- c
    )

    def m = tms.getModel
    assert(m.get == Set(b, d))

    tms.add(AspFact(a))
    assert(m.get == Set(a, c, d))

    //tms.forceChoiceOrder(Seq(b,d,c,a))
    //tms.forceChoiceOrder(Seq(d,b,c,a))
    //tms.forceChoiceOrder(Seq(c,d))

    var failures = 0

    times foreach { _ =>

      tms.remove(AspFact(a))

      if (failsToCompute(tms, Set(b,d))) failures += 1

      tms.add(AspFact(a))

    }

    println("failures: "+failures)
    printAvoidanceMap(tms)
  }

  test("jtms5 essence part") {

    val tms = JtmsLearn(AspProgram(
      AspRule(a, c), //a :- c
      AspRule(c, a), //c :- a
      AspRule(b, none, Set(a)), //b :- not a
      AspRule(d, b))) //d :- b)

    def m = tms.getModel.get

    tms.add(AspFact(a))
    assert(m == Set(a, c))

    var failures = 0

    times foreach { _ =>

      tms.remove(AspFact(a))

      if (failsToCompute(tms, Set(b,d))) failures += 1

      tms.add(AspFact(a))

    }

    println("failures: "+failures)
    printAvoidanceMap(tms)
  }

  test("jtms5-like problem for add") {

    val tms = JtmsLearn(AspProgram(
      AspRule(c, a), //c :- a
      AspRule(b, none, Set(a)), //b :- not a
      AspRule(d, b), //d :- b
      AspRule(d, c)) //d :- c
    )

    def m = tms.getModel

    assert(m.get == Set(b, d))

    //println("\nadd a :- c")
    tms add AspRule(a, c) //a :- c

    var failures = 0
    times foreach { _ =>

      //println("\nadd a :- not e.")
      //tms forceChoiceOrder Seq(a)
      tms add AspRule(a, none, Set(e)) //a :- not e.  instead of AspFact(a)
      if (failsToCompute(tms, Set(a, c, d))) failures += 1

      //println("\nadd e.")
      //tms forceChoiceOrder Seq(c) //just saying "c first"
      //tms forceChoiceOrder Seq(d,c)
      tms add AspFact(e) //e.  instead of removing fact a directly

      if (failsToCompute(tms, Set(e, b, d))) failures += 1

      tms remove AspFact(e)

      if (failsToCompute(tms, Set(a, c, d))) failures += 1

      tms remove AspRule(a, none, Set(e))

      if (failsToCompute(tms, Set(b, d))) failures += 1

    }

    println("failures: "+failures)
    printAvoidanceMap(tms)
  }

  test("jtms5 variant with direct dependency of body atoms for same head") {

    val tms = JtmsLearn(AspProgram(
      AspRule(d, b), //d :- b
      AspRule(d, c), //d :- c
      AspRule(c, none, Set(b)), //c :- not b. these are the crucial three rules,
      // the other exist only to make them initially unknown s.t. fixOut kicks in for d
      AspRule(b, none, Set(c)), //b :- not c. his rule is only needed s.t. b is not determined after the input "a" later
      AspRule(b, none, Set(a)), //b :- not a
      AspRule(c, none, Set(a))) //c :- not a
    )

    def m = tms.getModel.get

    assert(m == Set(b, c, d))

    var failures = 0
    times foreach { _ =>

      tms.add(AspFact(a))

      if (failsToCompute(tms, m == Set(a,b,d) || m == Set(a,c,d))) failures += 1

      tms.remove(AspFact(a))

    }

    println("failures: "+failures)
    printAvoidanceMap(tms)
  }


  test("constraint x :- a,b, not x.") {

    val tms = JtmsLearn(AspProgram())
    //val tms = JtmsGreedy(AspProgram())
    //val tms = JtmsDoyle(AspProgram())

    /*
      a :- b, not c.
      a :- e.
      b :- not d.
      d :- not a.
      d :- c, e.  => {a,b} | {d}
      x :- a,b, not x. ==> {d}
     */

    def m = tms.getModel.get

    tms add AspRule(a, Set(b), Set(c))
    tms add AspRule(a, e)
    tms add AspRule(b, none, Set(d))
    tms add AspRule(d, none, Set(a))
    tms add AspRule(d, Set(c,e))

    assert(m == Set(a,b)) // || m == Set(d))

    var failures = 0
    times foreach { _ =>

      tms add AspRule(x,Set(a,b),Set(x))

      if (failsToCompute(tms,Set(d))) failures += 1

      tms remove AspRule(x,Set(a,b),Set(x))

      tms remove AspRule(d, none, Set(a))

      if (failsToCompute(tms,Set(a,b))) failures += 1

      tms add AspRule(d, none, Set(a))

      assert(m == Set(a,b))
      tms.shuffle = true

    }

    println("failures: "+failures)
    printAvoidanceMap(tms)

    if (failures > 4) assert(false)
  }

  test("reach") {

    val tms = new JtmsLearn()

    val a = "a"
    val b = "b"
    val c = "c"
    val d = "d"
    val e = "e"
    def edge(x: String, y: String) = Atom("edge(" + x + "," + y + ")")
    def reach(x: String, y: String) = Atom("reach(" + x + "," + y + ")")
    def blocked(x: String, y: String) = Atom("blocked(" + x + "," + y + ")")

    tms.add(edge(a, b))
    tms.add(edge(b, c))
    tms.add(edge(c, d))
    tms.add(edge(d, e))
    tms.add(edge(b, e))

    val C = List(a, b, c, d, e)
    //reach(X,Y) :- edge(X,Y), not blocked(X,Y).
    for (x <- C) {
      for (y <- C) {
        val r = AspRule(reach(x, y), Set(edge(x, y)), Set(blocked(x, y)))
        tms.add(r)
        //println(r)
      }
    }
    //reach(X,Y) :- reach(X,Z), edge(Z,Y), not blocked(Z,Y).
    for (x <- C) {
      for (y <- C) {
        for (z <- C) {
          val r = AspRule(reach(x, y), Set(reach(x, z), edge(z, y)), Set(blocked(z, y)))
          tms.add(r)
          //println(r)
        }
      }
    }

    def m = tms.getModel.get

    assert(m contains reach(a, b))
    assert(m contains reach(b, c))
    assert(m contains reach(c, d))
    assert(m contains reach(d, e))
    assert(m contains reach(a, c))
    assert(m contains reach(a, d))
    assert(m contains reach(a, e))
    assert(m contains reach(b, c))
    assert(m contains reach(b, d))
    assert(m contains reach(b, e))
    assert(m contains reach(c, d))
    assert(m contains reach(c, e))
    assert(m contains reach(d, e))

    //println("active rules: "+tms.activeRules())
    //println("\ninactive rules: "+tms.inactiveRules())

    times foreach { _ =>

      /* edge(a,b). edge(b,c). edge(c,d). edge(d,e). edge(b,e).
         reach(X,Y) :- edge(X,Y), not blocked(X,Y).
         reach(X,Y) :- reach(X,Z), edge(Z,Y), not blocked(Z,Y).
    */

      tms.add(blocked(b, c))
      assert(!(m contains reach(b, d)))
      assert(m contains reach(b, e))
      assert(m contains reach(c, d))
      assert(m contains reach(c, e))
      assert(m contains reach(a, e))

      tms.add(blocked(b, e))
      assert(!(m contains reach(b, e)))
      assert(m contains reach(d, e))
      assert(m contains reach(c, e))
      assert(!(m contains reach(a, e)))

      tms.remove(blocked(b, c))
      assert(m contains reach(b, c))
      assert(m contains reach(b, d))
      assert(m contains reach(b, e))
      assert(m contains reach(a, e))

      tms.remove(blocked(b, e))

    }
  }

  test("a :- not b. b :- not a. ...") {

    // needs prevState!

    val tms = new JtmsLearn()
    //val tms = new JtmsGreedy()

    def m = tms.getModel

    tms.add(AspRule(a, none, Set(b)))
    tms.add(AspRule(b, none, Set(a)))
    assert(m.get == Set(a))

    var failures = 0
    times foreach { _ =>

      tms add AspRule(x,Set(a),Set(x))  //  :- a

      if (failsToCompute(tms,Set(b)))
        failures += 1

      tms add AspRule(y,Set(b),Set(y)) // :- b

      assert(m == None)

      tms remove AspRule(x,Set(a),Set(x))  //del :- a

      if (failsToCompute(tms,Set(a)))
        failures += 1

      tms remove AspRule(y,Set(b),Set(y))

      if (failsToCompute(tms,m.get == Set(a) || m.get == Set(b)))
        failures += 1

      tms add AspFact(a)

      if (failsToCompute(tms,Set(a)))
        failures += 1

      tms remove AspFact(a)

      if (failsToCompute(tms,m.get == Set(a) || m.get == Set(b)))
        failures += 1

    }

    println("failures: "+failures)
    printAvoidanceMap(tms)

    if (failures > 6) assert(false)
  }

  //illustrates the problem of finding a smart state for the avoidance map.
  //with changing rules (due to grounding temporal information), there is a different
  //set of rules at every time point and the naive concepts fails
  test("stream ds") {

    val wd = Atom("wd")

    val tms = JtmsLearn(AspProgram(
      AspRule(b, none, Set(c)), //b :- not c
      AspRule(c, none, Set(b)), //c :- not b
      AspRule(x, Set(a,b), Set(x)), // x :- a,b, not x
      AspRule(a, wd) //a <- \window^2 \Diamond d; add additional rules wd <- d(t) on the fly
    ))

    println(tms)

    def m = tms.getModel
    assert(m.get == Set(b) || m.get == Set(c))

    var failures = 0

    tms add AspFact(d(0))
    tms add AspRule(wd,d(0))  //t - 2
    tms add AspRule(wd,d(1)) //t - 1
    tms add AspRule(wd,d(2)) //t

    assert(tms.dataIndependentRules().toSet ==
      Set(AspRule(b, none, Set(c)),
          AspRule(c, none, Set(b)),
          AspRule(x, Set(a,b), Set(x)), // x :- a,b, not x
          AspRule(a, wd)))

//    println("data independent rules")
//    tms.dataIndependentRules() foreach println
//
//    println("\ndata dependent rules")
//    (tms.rules.toSet diff tms.dataIndependentRules().toSet) foreach println

    val intervalA = 10

    var lastD: Atom = d(0)

    for (t <- 3 to 50) {

      // a <- \window^2 \Diamond d
      // => remove wd <- d(t-3), add wd <- d(t)

      val ruleToAdd = AspRule(wd,d(t))
      tms add ruleToAdd

      val ruleToRemove = AspRule(wd,d(t-3))
      tms remove ruleToRemove

      if (t % intervalA == 0) {
        val fact: NormalFact = AspFact(d(t))
        tms.add(fact)
        lastD = fact.head
        assert(!tms.dataIndependentRules().contains(fact))
        assert(tms.facts().toSet.contains(fact))
      }
      //removing old data
      if (t % intervalA == 3) {
        tms.remove(d(t-3)) //this is not tms semantics, but assuming that data is deleted when it became irrelevant wrt potential inferences
      }

      println(t+": "+m.getOrElse(None))

      if (t % intervalA >= 0 && t % intervalA <= 2) {
        if (failsToCompute(tms, m.get == Set(b,lastD) || m.get == Set(a,c,lastD,wd))) failures += 1
      } else if (t % intervalA >= 3 && t % intervalA <= (intervalA-1)) {
        if (failsToCompute(tms, m.get == Set(b) || m.get == Set(c))) failures += 1
      }

    }

    println("failures: "+failures)
    printAvoidanceMap(tms)
  }

}
