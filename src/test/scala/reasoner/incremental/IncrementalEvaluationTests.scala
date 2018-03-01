package reasoner.incremental

import core._
import core.lars._
import fixtures.JtmsIncrementalReasoner
import org.scalatest.FunSuite
import reasoner.Reasoner


/**
  * Created by hb on 01.03.18.
  *
  * Some tests used also in performance evaluation. Here semantic tests.
  */
class IncrementalEvaluationTests extends FunSuite with JtmsIncrementalReasoner {

  def containsWithReasoner(reasoner: Reasoner)(t: Long, atom: Atom): Unit = {
    assert(reasoner.evaluate(t).model.contains(atom))
  }

  def notContainsWithReasoner(reasoner: Reasoner)(t: Long, atom: Atom): Unit = {
    assert(!reasoner.evaluate(t).model.contains(atom))
  }

  def emptyInReasoner(reasoner: Reasoner)(t: Long): Unit = {
    assert(reasoner.evaluate(t).model.isEmpty)
  }

  //

  test("cars 1") {

    val n=10 //time window size
    val k=5 //threshold of nr of cars
    val nrOfCars = 20

    //needed for test:
    assert(n > 0)
    assert(nrOfCars > k)

    val moreThanK = Atom(Predicate("moreThanK"))
    val x = Atom(Predicate("x"))
    val C = StringVariable("C")
    val carC = Atom(Predicate("car"),Seq(C))
    val recC = Atom(Predicate("rec"),Seq(C))
    val T = StringVariable("T")

    def car(i: Int) = Atom(Predicate("car"),Seq(""+i))
    def rec(i: Int) = Atom(Predicate("rec"),Seq(""+i))

    val carFacts: Set[LarsRule] = (1 to nrOfCars) map { i => LarsFact(car(i)) } toSet

    val program = LarsProgram.from(
      moreThanK <= WindowAtom(TimeWindow(n), Diamond, x),
      AtAtom(T,x) <= carC and WindowAtom(TupleWindow(k+1), At(T), recC) not WindowAtom(TupleWindow(k), Diamond, recC)
    ) ++ LarsProgram.from(carFacts)

    val reasoner = reasonerBuilder(program)

    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    //all cars appear on the first time point
    var t = 1L
    (1 to nrOfCars) foreach { i => append(t,rec(i)) }
    has(t,moreThanK)

    t = t+n //previous t still reachable within time window
    has(t,moreThanK)

    t = t+1 //not reachable anymore
    hasN(t,moreThanK)

    (1 to k) foreach { i => append(t,rec(i)) }
    hasN(t,moreThanK)

    t = t+n
    append(t,rec(k+1))
    has(t,moreThanK)

    t = t+1
    hasN(t,moreThanK)

  }

  test("cars 2") {

    val n=10 //time window size
    val k=2 //threshold of nr of cars
    val nrOfCars = 3
    val speedValues = Seq(10,20,30,40,50)

    //needed for test:
    assert(n > 0)
    assert(nrOfCars > k)

    val moreThanK = Atom(Predicate("moreThanK"))
    val C = StringVariable("C")
    val C1 = StringVariable("C'")
    val C2 = StringVariable("C''")
    val N = StringVariable("N")
    val N1 = StringVariable("N'")
    val S = StringVariable("S")

    def mkArg(x: Any) = x match {
      case i:Integer => IntValue(i)
      case a:Argument => a
      case _ => fail()
    }

    //the follow three atom builds are used for variables and integers
    def car(x: Any) = Atom(Predicate("car"),Seq(mkArg(x)))
    def speed(x: Any) = Atom(Predicate("speed"),Seq(mkArg(x)))
    def speedRec(x: Any, y: Any) = Atom(Predicate("speedRec"),Seq(mkArg(x),mkArg(y)))

    def rec(x: Argument) = Atom(Predicate("rec"),Seq(x))
    def nextCand(x: Argument, y: Argument) = Atom(Predicate("next-cand"),Seq(x,y))
    def nNext(x: Argument, y: Argument) = Atom(Predicate("n-next"),Seq(x,y))
    def next(x: Argument, y: Argument) = Atom(Predicate("next"),Seq(x,y))
    def nFirst(x: Argument) = Atom(Predicate("n-first"),Seq(x))
    def first(x: Argument) = Atom(Predicate("first"),Seq(x))
    def nr(x: Argument, y: Argument) = Atom(Predicate("nr"),Seq(x,y))
    def int(x: Argument) = Atom(Predicate("int"),Seq(x))
    def thresh(x: Argument) = Atom(Predicate("thresh"),Seq(x))

    val carFacts: Set[LarsRule] = (1 to nrOfCars) map { i => LarsFact(car(i)) } toSet
    val speedFacts: Set[LarsRule] = speedValues map { i => LarsFact(speed(i)) } toSet
    val intFacts: Set[LarsRule] = (1 to nrOfCars) map { i => LarsFact(int(IntValue(i))) } toSet

    val program = LarsProgram.from(
      rec(C) <= car(C) and speed(S) and WindowAtom(TimeWindow(n),Diamond,speedRec(C,S)) and Geq(S,IntValue(30)),
      nextCand(C,C1) <= rec(C) and rec(C1) and Lt(C,C1),
      nNext(C,C1) <= nextCand(C,C2) and nextCand(C2,C1),
      next(C,C1) <= nextCand(C,C1) not nNext(C,C1),
      nFirst(C) <= rec(C) and nextCand(C1,C),
      first(C) <= rec(C) not nFirst(C),
      nr(C,IntValue(1)) <= first(C),
      nr(C1,N1) <= nr(C,N) and next(C,C1) and int(N1) and Incr(N,N1),
      moreThanK <= nr(C,N) and thresh(IntValue(k)) and Gt(N,IntValue(k)),
      LarsFact(thresh(IntValue(k)))
    ) ++ LarsProgram.from(carFacts ++ speedFacts ++ intFacts)

    val reasoner = reasonerBuilder(program)

    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    //all cars appear on the first time point with proper speed
    var t = 1L
    (1 to nrOfCars) foreach { c => append(t,speedRec(c,30)) }
    has(t,moreThanK)

    t = t+n //previous t still reachable within time window
    has(t,moreThanK)

    t = t+1 //not reachable anymore
    hasN(t,moreThanK)

    (1 to k) foreach { c => append(t,speedRec(c,30)) }
    hasN(t,moreThanK)

    t = t+n
    append(t,speedRec(k+1,30))
    has(t,moreThanK)

    t = t+1
    hasN(t,moreThanK)

  }


}
