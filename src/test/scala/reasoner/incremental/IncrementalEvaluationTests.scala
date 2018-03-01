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


}
