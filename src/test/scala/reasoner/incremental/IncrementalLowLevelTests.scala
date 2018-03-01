package reasoner.incremental

import core._
import core.lars._
import fixtures.JtmsIncrementalReasoner
import org.scalatest.FunSuite
import reasoner.Reasoner




/**
  * Created by hb on 08.02.18.
  */
class IncrementalLowLevelTests extends FunSuite with JtmsIncrementalReasoner {

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

  val b = Atom(Predicate("b"))
  val c = Atom(Predicate("c"))
  val d = Atom(Predicate("d"))
  val e = Atom(Predicate("e"))
  val f = Atom(Predicate("f"))
  val h = Atom(Predicate("h"))
  val U = Variable("U")

  test("basic input handling") {
    val program = LarsProgram.from(
      h <= b
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(5,b); has(5,h)
    //model does not contain b, since b is not a signal by convention;
    //signals are assumed to be in the scope of windows

    for (t <- 6 to 9){
      empty(t)
    }

    append(10,b); has(10,h)
    append(10,b); has(10,h) //redundant
    append(11,b);  has(11,h)
    empty(12)

  }

  test("time diamond") {

    val program = LarsProgram.from(
      h <= WindowAtom(TimeWindow(2), Diamond, b)
    )
    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); has(3,h)
    hasN(4,b); has(4,h)
    hasN(5,b); has(5,h)
    empty(6)
    empty(7)
    append(8,b); has(8,b); has(8,h)
    append(8,b); has(8,b); has(8,h) //redundant
    append(9,b); has(9,b); has(9,h)
    hasN(10,b); has(10,h)

    append(12,b); has(12,b); has(12,h)
    hasN(13,b); has(13,h)
    hasN(14,b); has(14,h)
    empty(15)

    //

    append(15,c); hasN(15,h)
    append(15,b); has(15,h)
    append(15,d); has(15,h)
    append(15,e); has(15,h)
    has(16,h)
    has(17,h)
    hasN(18,h)

  }

  test("time box") {

    val program = LarsProgram.from(
      h <= WindowAtom(TimeWindow(2), Box, b)
    )
    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); hasN(3,h)
    hasN(4,b); hasN(4,h)
    empty(5)
    empty(6)

    append(8,b); has(8,b); hasN(8,h) //#1
    append(9,b); has(9,b); hasN(9,h)//#2
    append(10,b); has(10,b); has(10,h)//#3
    append(11,b); has(11,b); has(11,h)//#4
    append(11,c); has(11,h)
    append(11,d); has(11,h)
    append(11,e); has(11,h)
    empty(12)
    empty(13)

  }

  test("time at") {

    val program = LarsProgram.from(
      h <= WindowAtom(TimeWindow(2), At(U), b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); has(3,h)
    hasN(4,b); has(4,h)
    hasN(5,b); has(5,h)
    empty(6)
    empty(7)

    append(8,b); has(8,b); has(8,h)
    append(8,b); has(8,b); has(8,h) //redundant
    append(9,b); has(9,b); has(9,h)
    hasN(10,b); has(10,h)

    append(12,b); has(12,b); has(12,h)
    hasN(13,b); has(13,h);
    hasN(14,b); has(14,h)
    empty(15)

    //

    append(15,c); hasN(15,h)
    append(15,b); has(15,h)
    append(15,d); has(15,h)
    append(15,e); has(15,h)
    has(16,h)
    has(17,h)
    hasN(18,h)

  }

  test("time at 2") {

    val program = LarsProgram.from(
      h <= WindowAtom(TimeWindow(2), At(U), b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); has(3,h) //#1
    hasN(4,b); has(4,h)
    hasN(5,b); has(5,h)
    hasN(6,b); hasN(6,h)
    hasN(7,b); hasN(7,h)
    append(8,c); hasN(8,b); hasN(8,h) //#2
    hasN(8,b); hasN(8,h);
    append(9,d); hasN(9,h) //#3
    append(9,e); hasN(9,h) //#4
    append(9,b); has(9,h) //#5
    has(10,h)
    has(11,h)
    append(11,c); has(11,h) //#6
    append(11,d); has(11,h)
    append(11,e); has(11,h)
    append(12,b); has(12,h)
    append(12,c); has(12,h)
    append(12,d); has(12,h)
    append(12,e); has(12,h)
    has(13,h)
    has(14,h)
    hasN(15,h)

  }


  test("time at - head") {

    val program = LarsProgram.from(
      AtAtom(U,h) <= WindowAtom(TimeWindow(2), At(U), b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); has(3,h)
    hasN(4,b); hasN(4,h)
    hasN(5,b); hasN(5,h)
    empty(6); empty(7)
    append(8,b); has(8,b); has(8,h)
    append(8,b); has(8,b); has(8,h) //redundant
    append(9,b); has(9,b); has(9,h)
    hasN(10,b); hasN(10,h)

    append(12,b); has(12,b); has(12,h)
    hasN(13,b); hasN(13,h)
    hasN(14,b); hasN(14,h)
    empty(15)

    //

    append(15,c); hasN(15,h)
    append(15,b); has(15,h)
    append(15,d); has(15,h)
    append(15,e); has(15,h)
    hasN(16,h)
    hasN(17,h)
    hasN(18,h)

  }

  test("tuple diamond") {

    val program = LarsProgram.from(
      h <= WindowAtom(TupleWindow(2), Diamond, b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); has(3,h)
    hasN(4,b); has(4,h); hasN(5,b)
    has(5,h); hasN(6,b); has(6,h)
    hasN(7,b); has(7,h);
    append(8,c); hasN(8,b); has(8,h) //has(8,c)) in jtms model, but not a signal
    hasN(9,b); has(9,h)
    append(9,d); hasN(9,h)
    append(9,e); hasN(9,h)
    append(9,b); has(9,h)
    has(10,h)
    has(11,h)
    append(11,c); has(11,h)
    append(11,d); hasN(11,h)
    append(11,e); hasN(11,h)
    append(12,b); has(12,h)
    append(12,c); has(12,h)
    append(12,d); hasN(12,h)
    append(12,e); hasN(12,h)

    //set semantics! strictly speaking addition must be rejected
    //append(12,b)
    //hasN(12,h)

  }

  test("tuple at") {

    val program = LarsProgram.from(
      h <= WindowAtom(TupleWindow(2), At(U), b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); has(3,h) //#1
    hasN(4,b); has(4,h);
    hasN(5,b); has(5,h)
    hasN(6,b); has(6,h)
    hasN(7,b); has(7,h)
    append(8,c); hasN(8,b); has(8,h) //#2
    hasN(8,b); has(8,h)
    append(9,d); hasN(9,h) //#3
    append(9,e); hasN(9,h) //#4
    append(9,b); has(9,h) //#5
    has(10,h)
    has(11,h)

    /**
      * requires special treatment.
      * smart expiration of base rule leads here to a corner case.
      * keeping it for 2 tuples does not suffice.
      * h :- w(9) will be deleted first and re-inserted.
      * here, however, we expire it due to atom e at time 9,
      * but we would still need it due to the later atom b at time 9.
      * in contrast to before, we now add at time 11, which leads to
      * h :- w(11), and the incremental rule w(9) :- b#(9,5) has nothing
      * left to trigger
      */
    append(11,c) //#6
    has(11,h)

    append(11,d); hasN(11,h)
    append(11,e); hasN(11,h)
    append(12,b); has(12,h)
    append(12,c); has(12,h)
    append(12,d); hasN(12,h)
    append(12,e); hasN(12,h)

  }

  test("tuple at - head") {

    val program = LarsProgram.from(
      AtAtom(U,h) <= WindowAtom(TupleWindow(2), At(U), b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); has(3,h) //#1
    empty(4)
    empty(5)
    empty(6)
    empty(7)
    append(8,c); empty(8) //#2
    empty(9)
    append(9,d); hasN(9,h) //#3
    append(9,e); hasN(9,h) //#4
    append(9,b); has(9,h) //#5
    hasN(10,h)
    hasN(11,h)

    //(see note for "tuple at")
    append(11,c); hasN(11,h) //#6
    append(11,d); hasN(11,h)
    append(11,e); hasN(11,h)
    append(12,b); has(12,h)
    append(12,c); has(12,h)
    append(12,d); hasN(12,h)
    append(12,e); hasN(12,h)

  }

  test("tuple box - size 2") {

    val program = LarsProgram.from(
      h <= WindowAtom(TupleWindow(2), Box, b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,b); has(3,b); hasN(3,h)
    empty(4)
    empty(5)
    empty(6)
    append(8,b); has(8,b); hasN(8,h) //#1
    append(9,b); has(9,b); has(9,h) //#2
    append(10,b); has(10,b); has(10,h) //#3
    append(11,b); has(11,b); has(11,h) //#4
    append(11,c); has(11,h)//#5
    append(11,d); has(11,b); hasN(11,h) //#6
    append(11,e); hasN(11,h) //#7
    empty(12)
    empty(13)
    append(15,c); hasN(15,h)
    append(15,b); has(15,h)
    append(16,b); has(16,h)
    append(17,b); has(17,h)
    append(17,c); has(17,h)
    append(18,b); hasN(18,h)
    append(19,b); has(19,h)
    hasN(20,h)

  }

  test("tuple box - size 3") {

    val program = LarsProgram.from(
      h <= WindowAtom(TupleWindow(3), Box, b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(1,b); has(1,b); hasN(1,h)
    append(2,b); hasN(2,h)
    append(3,b); has(3,h)
    append(3,c); has(3,h)
    append(3,d); has(3,h)
    append(3,e); hasN(3,h)
    append(4,b); hasN(4,h)
    append(5,b); hasN(5,h)
    append(5,c); has(5,h)
    append(6,b); has(6,h)
    append(7,b); hasN(7,h)
    append(8,b); has(8,h)
    hasN(9,h)

  }

  test("tuple box - size 4") {

    val program = LarsProgram.from(
      h <= WindowAtom(TupleWindow(4), Box, b)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)
    def yes(t: Long) = containsWithReasoner(reasoner)(t,h)
    def no(t: Long) = notContainsWithReasoner(reasoner)(t,h)

    no(1)
    append(1,c); no(1)
    append(1,d); no(1)
    append(1,e); no(1)
    append(1,b); yes(1)

    append(2,b); yes(2)
    append(2,c); yes(2)
    append(2,d); yes(2)
    append(2,e); yes(2)
    append(2,f); no(2)

    no(3)
    append(3,c); no(3)
    append(3,d); no(3)
    append(3,e); no(3)
    append(3,b); yes(3)

    append(4,c); no(4)
    append(4,d); no(4)
    append(4,b); yes(4)
    append(4,e); yes(4)

    append(5,b); yes(5)

    append(6,b); yes(6)

    append(7,b); no(7)

    append(8,b); yes(8)
    append(8,c); yes(8)
    append(8,d); yes(8)
    append(8,e); yes(8)

    append(9,b); no(9)
    append(9,c); no(9)
    append(9,d); no(9)
    append(9,e); yes(9)

    append(10,c); no(10)
    append(10,d); no(10)
    append(10,e); no(10)
    append(10,b); yes(10)

    append(11,b); yes(11)

    append(12,b); yes(12)

    append(13,b); yes(13)

    append(14,b); yes(14)

    no(15)

  }




}
