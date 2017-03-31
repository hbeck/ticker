package jtms.asp.examples

import core.asp.{AspFact, AspProgram, AspRule, NormalFact}
import core._
import fixtures.AtomTestFixture
import jtms._
import jtms.algorithms.{JtmsGreedy, JtmsLearn}
import org.scalatest.FunSuite

import scala.collection.immutable.HashMap

/**
  * Created by hb on 2016-07-31
  */
class JtmsLearnTests extends FunSuite with AtomTestFixture {
  
  //def jtmsImpl = JtmsDoyle
  def jtmsImpl = JtmsGreedy  

  val none = Set[Atom]()

  val timesUpper = 500

  val times = 1 to timesUpper

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
    
    printAvoidanceMap(tms)
    println("\nfailures: "+failures)

  }

  def printAvoidanceMap(tms: JtmsUpdateAlgorithm): Unit = {
    if (!tms.isInstanceOf[JtmsLearn]) return
    val jtms = tms.asInstanceOf[JtmsLearn]
    println(jtms.tabu)
  }


  //returns true if failure
  def failsToCompute(tms: JtmsUpdateAlgorithm, model: Set[Atom]): Boolean = {
    if (tms.getModel == None) {
//      if (tms.isInstanceOf[JtmsLearn]) {
//        val jtms = tms.asInstanceOf[JtmsLearn]
//        println()
//        println(jtms.state)
//        println("sel. atom: "+jtms.selectedAtom+"\n")
//      }
      return true
    } else {
      assert(tms.getModel.get == model)
      return false
    }
  }

  def failsToCompute(tms: JtmsUpdateAlgorithm, condition: => Boolean): Boolean = {
    if (tms.getModel == None) {
//      if (tms.isInstanceOf[JtmsLearn]) {
//        val jtms = tms.asInstanceOf[JtmsLearn]
//        println(jtms.state)
//        println("sel. atom: "+jtms.selectedAtom+"\n")
//      }
      return true
    } else {
      if (!condition) {
        print("model: " + tms.getModel.getOrElse(None))
        assert(false)
      }
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
    
    printAvoidanceMap(tms)
    println("\nfailures: "+failures)
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

    
    printAvoidanceMap(tms)
    println("\nfailures: "+failures)
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

    
    printAvoidanceMap(tms)
    println("\nfailures: "+failures)
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

    
    printAvoidanceMap(tms)
    println("\nfailures: "+failures)
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

    }

    
    printAvoidanceMap(tms)
    println("\nfailures: "+failures)

    if (tms.isInstanceOf[JtmsLearn]) {
      if (failures > 4) assert(false)
    }
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

  test("reach sampling") {
    test_reach_sampling()
  }

  test("performance reach sampling") {
    pending
    performance_test(10,test_reach_sampling())
  }

  def test_reach_sampling():Unit = {

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

    val atom: Map[Int,String] = new HashMap[Int,String]() + (0 -> "a") + (1 -> "b") + (2 -> "c") + (3 -> "d") +(4 -> "e")

    times foreach { _ =>

      /* edge(a,b). edge(b,c). edge(c,d). edge(d,e). edge(b,e).
         reach(X,Y) :- edge(X,Y), not blocked(X,Y).
         reach(X,Y) :- reach(X,Z), edge(Z,Y), not blocked(Z,Y).
    */

      val n1 = tms.random.nextInt(5)
      val diff = 4-n1
      val n2 = n1 + tms.random.nextInt(diff+1)

      if (tms.random.nextDouble < 0.5) tms add blocked(atom(n1),atom(n2))
      else tms add blocked(atom(n1), atom(n2))

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
    
    printAvoidanceMap(tms)
    println("\nfailures: "+failures)

    if (tms.isInstanceOf[JtmsLearn]) {
      if (failures > 6) assert(false)
    }
  }

  //illustrates the problem of finding a smart state for the avoidance map.
  //with changing rules (due to grounding temporal information), there is a different
  //set of rules at every time point and the naive concepts fails
  test("streaming 1 analytic") {

    val tms = JtmsLearn(AspProgram(
      AspRule(b, none, Set(c)), //b :- not c
      AspRule(c, none, Set(b)), //c :- not b
      AspRule(x, Set(a,b), Set(x)), // x :- a,b, not x
      AspRule(a, waux_d), //a <- \window^2 \Diamond d; add additional rules waux_d <- d(t) on the fly
      AspRule(b, waux_e) //b <- e; translating to  b <- \window^0 \Diamond e
    ))

    println(tms)

    def m = tms.getModel
    assert(m.get == Set(b) || m.get == Set(c))

    var failures = 0

    assert(tms.jtms.dataIndependentRules.toSet ==
       Set(AspRule(b, none, Set(c)),
           AspRule(c, none, Set(b)),
           AspRule(x, Set(a,b), Set(x)), // x :- a,b, not x
           AspRule(a, waux_d),
           AspRule(b, waux_e)))

    var lastD: Atom = d(0)
    var lastE: Atom = e(-1)

    for (t <- 0 to 1000) {

      // a <- \window^2 \Diamond d
      // => add waux_d <- d(t), remove waux_d <- d(t-3)

      val dRuleToAdd = AspRule(waux_d,d(t))
      tms add dRuleToAdd

      val dRuleToRemove = AspRule(waux_d,d(t-3))
      tms remove dRuleToRemove

      // b <- \window^0 \Diamond e
      // => add waux_e <- e(t), remove waux_e <- e(t-1)

      val eRuleToAdd = AspRule(waux_e,e(t))
      tms add eRuleToAdd

      val eRuleToRemove = AspRule(waux_e,e(t-1))
      tms remove eRuleToRemove

      if (t % 10 == 0) {
        val fact: NormalFact = AspFact(d(t))
        tms.add(fact)
        lastD = fact.head
        assert(!tms.jtms.dataIndependentRules.contains(fact))
        assert(tms.jtms.facts.toSet.contains(fact))
      } else if (t % (10 / 2) == 0) {
        val fact: NormalFact = AspFact(e(t))
        tms.add(fact)
        lastE = fact.head
      }

      //removing old data
      //this is not tms semantics, but assuming that data is deleted when it became irrelevant wrt potential inferences
      if (t % 10 == 3) {
        tms.remove(d(t-3))
      } else if (t % (10 / 2) == 1) {
        tms.remove(e(t-1))
      }

      println(t+": "+m.getOrElse(None))

      // 0      1      2      3      4      5      6      7      8      9      0
      // d-------------------|
      //                                    e-----|
      // | {b,d} v {a,c,d,waux_d}| {b} v {c}   |{b,e} |       {b} v {c}           |

      if (t % 10 >= 0 && t % 10 <= 2) {
        if (failsToCompute(tms, m.get == Set(b,lastD) || m.get == Set(a,c,lastD,waux_d))) failures += 1
      } else if (t % 10 >= 3 && t % 10 < 5) {
        if (failsToCompute(tms, m.get == Set(b) || m.get == Set(c))) failures += 1
      } else if (t % 10 == 5) {
        if (failsToCompute(tms, Set(b,lastE,waux_e))) failures += 1
      } else if (t % 10 >= 6 && t % 10 <= 9) {
        if (failsToCompute(tms, m.get == Set(b) || m.get == Set(c))) failures += 1
      }

    }
    
    printAvoidanceMap(tms)
    println("\nfailures: "+failures)
  }

  //TODO: i want to have an access pinnedAtom.time.asInt
  def getTime(maybeAtom: Option[Atom]): Option[Int] = {
    maybeAtom match {
      case Some(atom) => Some(Integer.parseInt(atom.asInstanceOf[PinnedTimeAtom].time.toString))
      case None => None
    }
  }

  val waux_d = Atom("waux_d")
  val waux_e = Atom("waux_e")

  test("streaming 1 sampling") {
    test_streaming_1_sampling(false)
  }

  test("performance streaming 1 sampling ") {
    pending
    performance_test(10,test_streaming_1_sampling())
  }

  def performance_test(loops: Int, testCode: => Any): Unit = {
    println("runs: "+loops)
    println("inner loop: "+timesUpper)
    var totalTime: Long = 0
    for (i <- 0 to loops) { //exclude first iteration from stats
      val start = System.currentTimeMillis()
      //
      testCode
      //
      val end = System.currentTimeMillis()
      val dur = (end-start)
      println("run "+i+": "+((1.0*dur)/1000.0)+" sec")
      if (i>0) {
        totalTime += dur
      }
    }
    println("avg 1-"+loops+": "+((1.0*totalTime)/1000.0/(1.0*loops))+" sec per run")
  }

  def test_streaming_1_sampling(doPrint: Boolean = false): Unit = {

    var totalFailures = 0
    var totalNoModel = 0

    for (likelihood <- Seq(0.05, 0.1, 0.25, 0.5, 0.8, 0.95)) {

      if (doPrint) println("selection likelihood: "+likelihood)

      val tms = JtmsLearn(AspProgram(
        AspRule(b, none, Set(c)), //b :- not c
        AspRule(c, none, Set(b)), //c :- not b
        AspRule(x, Set(a, b), Set(x)), // x :- a,b, not x
        AspRule(a, waux_d), //a <- \window^2 \Diamond d; add additional rules waux_d <- d(t) on the fly
        AspRule(b, waux_e) //b <- e; translating to  b <- \window^0 \Diamond e
      ))

      def m = tms.getModel
      assert(m.get == Set(b) || m.get == Set(c))

      assert(tms.jtms.dataIndependentRules.toSet ==
        Set(AspRule(b, none, Set(c)),
          AspRule(c, none, Set(b)),
          AspRule(x, Set(a, b), Set(x)), // x :- a,b, not x
          AspRule(a, waux_d),
          AspRule(b, waux_e)))

      var lastD: Option[Atom] = None
      var lastE: Option[Atom] = None
      var dBefore: Option[Atom] = None
      var eBefore: Option[Atom] = None

      var failures = 0

      var noModel = 0 //counter for correct None returned model (due to inconsistency of program)

      times foreach { t =>

        //sampling part; always have one last d, resp. e

        //if (doPrint) print("\n"+t+": ")

        /*
          first add facts
         */

        var replaceD = false
        if (tms.random.nextDouble() < likelihood) {
          replaceD = true
          val fact: NormalFact = AspFact(d(t))
          //print(fact+" ")
          tms.add(fact)

          lastD = Some(fact.head)
        }
        var replaceE = false
        if (tms.random.nextDouble() < likelihood) {
          replaceE = true
          val fact: NormalFact = AspFact(e(t))
          //print(fact+" ")
          tms.add(fact)
          lastE = Some(fact.head)
        }

        /*
          then add new rules
         */

        // a <- \window^2 \Diamond d
        // => add waux_d <- d(t), remove waux_d <- d(t-3) (below)

        val dRuleToAdd = AspRule(waux_d, d(t))
        tms add dRuleToAdd

        // b <- \window^0 \Diamond e
        // => add waux_e <- e(t), remove waux_e <- e(t-1) (below)
        val eRuleToAdd = AspRule(waux_e, e(t))
        tms add eRuleToAdd

        /*
           then remove old rules
         */
        val dRuleToRemove = AspRule(waux_d, d(t - 3))
        tms remove dRuleToRemove

        val eRuleToRemove = AspRule(waux_e, e(t - 1))
        tms remove eRuleToRemove

        /*
           and old facts
         */
        if (replaceD) {
          if (dBefore.isDefined) tms.remove(dBefore.get)
          dBefore = lastD
        }
        if (replaceE) {
          if (eBefore.isDefined) tms.remove(eBefore.get)
          eBefore = lastE
        }

        //if (doPrint) println("  " + m.getOrElse(None))

        val base = Set[Atom]() ++
          (if (lastD.isDefined) Set(lastD.get) else Set()) ++
          (if (lastE.isDefined) Set(lastE.get) else Set())

        if (getTime(lastE) == Some(t)) {
          //the program is inconsistent if e currently holds, and d within the last 2 time units
          val dTime = getTime(lastD)
          if (dTime.isDefined && dTime.get >= (t - 2)) {
            assert(m == None)
            noModel += 1
          } else {
            if (failsToCompute(tms, base ++ Set(b, waux_e))) failures += 1
          }
        } else {
          val dTime = getTime(lastD)
          if (dTime.isDefined && dTime.get >= (t - 2)) {
            if (failsToCompute(tms, base ++ Set(waux_d, a, c))) failures += 1
          } else {
            if (failsToCompute(tms, m.get == base ++ Set(b) || m.get == base ++ Set(c))) failures += 1
          }
        }

      }

      totalFailures += failures
      totalNoModel += noModel

      if (doPrint) {
        //println("\nstreaming likelihood d,e: "+likelihood)
        //printAvoidanceMap(tms)
        println("\nnoModel: " + noModel) // + " ("+((1.0*noModel)/(1.0*times))+")")
        println("failures: " + failures) // + " ("+((1.0*failures)/(1.0*times))+")")
      }

    }

    println("\ntotal failures: "+totalFailures)
    //println("total noModel: "+totalNoModel)
  }


  test("streaming 2 analytic") {

    val w5_a = Atom("w5_a") //\window^5 \Diamond a
    val w1_Box_b = Atom("w1_Box_b") //\window^1 \Box b
    val w2_Box_b = Atom("w2_Box_b") //\window^2 \Box b
    val w5_c = Atom("w5_c") //\window^5 \Diamond c

    val tms = JtmsLearn(AspProgram(
      AspRule(x,none,Set(y,z)),
      AspRule(y,none,Set(x,z)),
      AspRule(z,none,Set(x,y)),
      AspRule(x,Set(w5_a),Set(w5_c)), //prepare x <- \window^5 \Diamond a, \naf \window^5 \Diamond c
      AspRule(y,w1_Box_b), //prepare y <- \window^1 \Box b
      AspRule(n,Set(w2_Box_b,w5_c),Set(n)) //prepare n <- \window^2 \Box b, \window^5 \Diamond c, \naf n
    ))

    def m = tms.getModel
    def M = tms.getModel.get

    var failures = 0

    var a1: Option[Atom] = None
    var b2: Option[Atom] = None
    var b3: Option[Atom] = None
    var b4: Option[Atom] = None
    var b8: Option[Atom] = None
    var b9: Option[Atom] = None
    var c13: Option[Atom] = None

    for (t <- 0 to 1000) {

      //prepare x <- \window^5 \Diamond a, \naf \window^5 \Diamond c
      //prepare y <- \window^1 \Box b
      //prepare n <- \window^2 \Box b, \window^5 \Diamond c, naf b

      tms add AspRule(w5_a,a(t)) // w5_a <- a(t)
      tms add AspRule(w5_c,c(t)) // w5_c <- c(t)
      tms add AspRule(w1_Box_b,Set[Atom](b(t-1),b(t))) //w1_Box_b <- b(t-1), b(t)
      tms add AspRule(w2_Box_b,Set[Atom](b(t-2),b(t-1),b(t))) //w2_Box_b <- b(t-2), b(t-1), b(t)

      tms remove AspRule(w5_a,Set[Atom](a(t-6)))
      tms remove AspRule(w5_c,Set[Atom](c(t-6)))
      tms remove AspRule(w1_Box_b,Set[Atom](b(t-2),b(t-1)))
      tms remove AspRule(w2_Box_b,Set[Atom](b(t-3),b(t-2),b(t-1)))

      if (t % 10 == 1) {
        val fact: NormalFact = AspFact(a(t))
        tms.add(fact)
        a1 = Some(fact.head)
      } else if (t % 10 == 2) {
        val fact: NormalFact = AspFact(b(t))
        tms.add(fact)
        b2 = Some(fact.head)
      } else if (t % 10 == 3) {
        val fact: NormalFact = AspFact(b(t))
        tms.add(fact)
        b3 = Some(fact.head)
        if (t % 20 == 13) {
          val fact: NormalFact = AspFact(c(t))
          tms.add(fact)
          c13 = Some(fact.head)
        }
      } else if (t % 10 == 4) {
        val fact: NormalFact = AspFact(b(t))
        tms.add(fact)
        b4 = Some(fact.head)
      } else if (t % 10 == 8) {
        val fact: NormalFact = AspFact(b(t))
        tms.add(fact)
        b8 = Some(fact.head)
      } else if (t % 10 == 9) {
        val fact: NormalFact = AspFact(b(t))
        tms.add(fact)
        b9 = Some(fact.head)
      }

      //removing old data
      if (t % 10 == 7) {
        tms.remove(a(t-6))
      }
      if (t % 10 >= 4 && t % 10 <= 7) {
        tms.remove(b(t-3))
      } else if (t % 10 >= 0 && t % 10 <= 2) {
        tms.remove(b(t-3))
      }
      if (t % 20 == 19) {
        tms.remove(c(t-6))
      }

      println(t+": "+m.getOrElse(None))

      // 0      1      2      3      4      5      6      7      8      9      0
      //        a----------------------------------------|
      //               b------b------b-----|                     b------b------|
      //                     (c---------------------------------|)

      //x | y | z.
      //x <- \window^5 \Diamond a, \naf \window^5 \Diamond c
      //y <- \window^1 \Box b
      //n <- \window^2 \Box b, \window^5 \Diamond c, \naf n

      if (t % 10 == 0) {
        val bSet: Set[Atom] = if (t>0) Set(b8.get,b9.get) else Set()
        if (failsToCompute(tms, M == bSet++Set(x) || M == bSet++Set(y) || M == bSet++Set(z))) failures += 1
      } else if (t % 10 == 1) {
        val bSet: Set[Atom] = if (t>1) Set(b9.get) else Set()
        if (failsToCompute(tms, bSet++Set(a1.get, w5_a, x))) failures += 1
      } else if (t % 10 == 2) {
        if (failsToCompute(tms, Set(a1.get, b2.get, w5_a, x))) failures += 1
      } else if (t % 10 == 3) {
        if (t % 20 == 13) {
          if (failsToCompute(tms, Set(a1.get, b2.get, b3.get, c13.get, w5_a, w1_Box_b, w5_c, y))) failures += 1
        } else {
          if (failsToCompute(tms, Set(a1.get, b2.get, b3.get, w5_a, w1_Box_b, x, y))) failures += 1
        }
      } else if (t % 10 == 4) {
        if (t % 20 == 14) {
          assert(m == None)
        } else {
          if (failsToCompute(tms, Set(a1.get, b2.get, b3.get, b4.get, w5_a, w1_Box_b, w2_Box_b, x, y))) failures += 1
        }
      } else if (t % 10 == 5) {
        if (t % 20 == 15) {
          val base = Set(a1.get, b3.get, b4.get, c13.get, w5_a, w5_c)
          if (failsToCompute(tms, M == base ++ Set(x) || M == base ++ Set(y) || M == base ++ Set(z))) failures += 1
        } else {
          if (failsToCompute(tms, Set(a1.get, b3.get, b4.get, w5_a, x))) failures += 1
        }
      } else if (t % 10 == 6) {
        if (t % 20 == 16) {
          val base = Set(a1.get, b4.get, c13.get, w5_a, w5_c)
          if (failsToCompute(tms, M == base ++ Set(x) || M == base ++ Set(y) || M == base ++ Set(z))) failures += 1
        } else {
          if (failsToCompute(tms, Set(a1.get, b4.get, w5_a, x))) failures += 1
        }
      } else if (t % 10 == 7) {
        if (t % 20 == 17) {
          if (failsToCompute(tms, M == Set(x, c13.get, w5_c) || M == Set(y, c13.get, w5_c) || M == Set(z, c13.get, w5_c))) failures += 1
        } else {
          if (failsToCompute(tms, M == Set(x) || M == Set(y) || M == Set(z))) failures += 1
        }
      } else if (t % 10 == 8) {
        if (t % 20 == 18) {
          if (failsToCompute(tms, M == Set(x, c13.get, w5_c, b8.get) || M == Set(y, c13.get, w5_c, b8.get) || M == Set(z, c13.get, w5_c, b8.get))) failures += 1
        } else {
          if (failsToCompute(tms, M == Set(x, b8.get) || M == Set(y, b8.get) || M == Set(z, b8.get))) failures += 1
        }
      } else if (t % 10 == 9) {
        if (failsToCompute(tms, Set(b8.get, b9.get, w1_Box_b, y))) failures += 1
      }

    }

    printAvoidanceMap(tms)
    println("\nfailures: "+failures)

  }

  test("streaming 2 sampling") {
    test_streaming_2_sampling(false)
  }

  test("performance streaming 2 sampling") {
    pending
    performance_test(10,test_streaming_2_sampling())
  }

  def test_streaming_2_sampling(doPrint: Boolean=false): Unit = {

    val w5_a = Atom("w5_a") //\window^5 \Diamond a
    val w1_Box_b = Atom("w1_Box_b") //\window^1 \Box b
    val w2_Box_b = Atom("w2_Box_b") //\window^2 \Box b
    val w5_c = Atom("w5_c") //\window^5 \Diamond c

    val tms = JtmsLearn(AspProgram(
      AspRule(x,none,Set(y,z)),
      AspRule(y,none,Set(x,z)),
      AspRule(z,none,Set(x,y)),
      AspRule(x,Set(w5_a),Set(w5_c)), //prepare x <- \window^5 \Diamond a, \naf \window^5 \Diamond c
      AspRule(y,w1_Box_b), //prepare y <- \window^1 \Box b
      AspRule(n,Set(w2_Box_b,w5_c),Set(n)) //prepare n <- \window^2 \Box b, \window^5 \Diamond c, \naf n
    ))

    def m = tms.getModel
    def M = tms.getModel.get

    var failures = 0
    var noModel = 0

    var lastA: Option[Atom] = None
    var lastC: Option[Atom] = None

    times foreach { t =>

      //prepare x <- \window^5 \Diamond a, \naf \window^5 \Diamond c
      //prepare y <- \window^1 \Box b
      //prepare n <- \window^2 \Box b, \window^5 \Diamond c, naf n

      /*
        1 add facts
       */
      if (tms.random.nextDouble < 0.10) {
        tms.add(a(t))
        lastA = Some(a(t))
      }
      if (tms.random.nextDouble < 0.10) {
        tms.add(c(t))
        lastC = Some(c(t))
      }
      if (tms.random.nextDouble < 0.5) {
        tms.add(b(t))
      }

      /*
        2. add rules
       */

      tms add AspRule(w5_a, a(t)) // w5_a <- a(t)
      tms add AspRule(w5_c, c(t)) // w5_c <- c(t)
      tms add AspRule(w1_Box_b, Set[Atom](b(t - 1), b(t))) //w1_Box_b <- b(t-1), b(t)
      tms add AspRule(w2_Box_b, Set[Atom](b(t - 2), b(t - 1), b(t))) //w2_Box_b <- b(t-2), b(t-1), b(t)

      /*
        3. remove rules
       */

      tms remove AspRule(w5_a, Set[Atom](a(t - 6)))
      tms remove AspRule(w5_c, Set[Atom](c(t - 6)))
      tms remove AspRule(w1_Box_b, Set[Atom](b(t - 2), b(t - 1)))
      tms remove AspRule(w2_Box_b, Set[Atom](b(t - 3), b(t - 2), b(t - 1)))

      /*
         4. remove old data
       */
      if (t % 10 == 0) {
        for (i <- 0 to 10) {
          val tp = (t - 10) - i
          for (atom <- Seq(a, b, c)) {
            tms.remove(atom(tp))
          }
        }
      }

      if (doPrint) print(t + ": " + m.getOrElse(None))

      //x | y | z.
      //x <- \window^5 \Diamond a, \naf \window^5 \Diamond c
      //y <- \window^1 \Box b
      //n <- \window^2 \Box b, \window^5 \Diamond c, \naf n

      if (m == None) {
        if (lastC.isDefined && getTime(lastC).get >= (t-5)) {
          val bSet: Set[Atom] = Set(b(t),b(t-1),b(t-2))
          if (bSet.forall(a => tms.jtms.factAtoms.contains(a))) {
            noModel += 1
            //println("\n\t"+tms.factAtoms)
          } else {
            failures += 1
          }
        } else {
          failures += 1
        }
      } else {
        val bSet: Set[Atom] = Set(b(t),b(t-1))
        if (bSet.forall(M.contains(_))) {
          def cond1 = Set[Atom](y,w1_Box_b).forall(M.contains(_))
          if (lastC.isDefined && (getTime(lastC).get >= (t-5))) {
            def cond2 = Set[Atom](b(t - 2), w2_Box_b).forall(!M.contains(_))
            if (failsToCompute(tms, cond1 && cond2)) failures += 1
          }
          else {
            if (failsToCompute(tms, cond1)) failures += 1
          }
        }
        if (lastA.isDefined && getTime(lastA).get >= (t-5)) {
          if (lastC.isDefined && getTime(lastC).get >= (t-5)) {
            def cond1 = Set[Atom](lastA.get,lastC.get,w5_a,w5_c).forall(M.contains(_))
            def cond2 = M.contains(x) || M.contains(y) || M.contains(z)
            if (failsToCompute(tms, cond1 && cond2)) failures += 1
          } else {
            def cond1 = Set[Atom](lastA.get,w5_a).forall(M.contains(_))
            def cond2 = M.contains(x)
            if (failsToCompute(tms, cond1 && cond2)) failures += 1
          }
        }
      }

    }

    if (doPrint) {

      printAvoidanceMap(tms)
      println("\nfailures: "+failures)
      println("\nnoModel:  "+noModel)

    }

  }


}
