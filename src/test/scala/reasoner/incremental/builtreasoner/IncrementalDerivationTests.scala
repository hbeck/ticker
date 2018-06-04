package reasoner.incremental.builtreasoner

import core._
import core.lars._
import fixtures.JtmsIncrementalReasoner
import org.scalatest.FunSuite
import reasoner.Reasoner


/**
  * Created by hb on 15.02.18.
  *
  * Using tests from IncrementalJoinTests, breaking apart conjunction in bodies there into single rules here.
  * Pattern: h :- bi, bj. into:
  * i :- bi.
  * j :- bj.
  * h :- i, j.
  */
class IncrementalDerivationTests extends FunSuite with JtmsIncrementalReasoner {

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

  val pb = Atom(Predicate("p"),Seq(StringValue("b")))
  val pc = Atom(Predicate("p"),Seq(StringValue("c")))
  val pd = Atom(Predicate("p"),Seq(StringValue("d")))
  val pe = Atom(Predicate("p"),Seq(StringValue("e")))
  val pf = Atom(Predicate("p"),Seq(StringValue("f")))

  val sb = Atom(Predicate("s"),Seq(StringValue("b")))
  val sc = Atom(Predicate("s"),Seq(StringValue("c")))
  val sd = Atom(Predicate("s"),Seq(StringValue("d")))
  val se = Atom(Predicate("s"),Seq(StringValue("e")))
  val sf = Atom(Predicate("s"),Seq(StringValue("f")))

  val hb = Atom(Predicate("h"),Seq(StringValue("b")))
  val hc = Atom(Predicate("h"),Seq(StringValue("c")))
  val hd = Atom(Predicate("h"),Seq(StringValue("d")))
  val he = Atom(Predicate("h"),Seq(StringValue("e")))
  val hf = Atom(Predicate("h"),Seq(StringValue("f")))

  val ib = Atom(Predicate("i"),Seq(StringValue("b")))
  val ic = Atom(Predicate("i"),Seq(StringValue("c")))
  val id = Atom(Predicate("i"),Seq(StringValue("d")))
  val ie = Atom(Predicate("i"),Seq(StringValue("e")))
  val i_f = Atom(Predicate("i"),Seq(StringValue("f")))

  val jb = Atom(Predicate("j"),Seq(StringValue("b")))
  val jc = Atom(Predicate("j"),Seq(StringValue("c")))
  val jd = Atom(Predicate("j"),Seq(StringValue("d")))
  val je = Atom(Predicate("j"),Seq(StringValue("e")))
  val jf = Atom(Predicate("j"),Seq(StringValue("f")))

  val gb = Atom(Predicate("g"),Seq(StringValue("b")))
  val gc = Atom(Predicate("g"),Seq(StringValue("c")))
  val gd = Atom(Predicate("g"),Seq(StringValue("d")))
  val ge = Atom(Predicate("g"),Seq(StringValue("e")))
  val gf = Atom(Predicate("g"),Seq(StringValue("f")))

  val pX = Atom(Predicate("p"),Seq(StringVariable("X")))
  val sX = Atom(Predicate("s"),Seq(StringVariable("X")))
  val hX = Atom(Predicate("h"),Seq(StringVariable("X")))
  val iX = Atom(Predicate("i"),Seq(StringVariable("X")))
  val jX = Atom(Predicate("j"),Seq(StringVariable("X")))
  val gX = Atom(Predicate("g"),Seq(StringVariable("X")))

  val guards = LarsProgram.from(LarsFact(gb),LarsFact(gc),LarsFact(gd),LarsFact(ge),LarsFact(gf))

  val U = Variable("U")
  val U1 = Variable("U1")
  val U2 = Variable("U2")

  test("time diamond x2") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TimeWindow(2), Diamond, pX),
      jX <= gX and WindowAtom(TimeWindow(4), Diamond, sX),
      hX <= iX and jX
    ) ++ guards
    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); hasN(1,hb)
    append(1,sb); has(1,hb)
    has(2,hb)
    has(3,hb)
    hasN(4,hb)
    append(4,pb); has(4,hb)
    has(5,hb)
    hasN(6,hb)

    append(8,pc); append(8,pd); append(8,pe);   hasN(8,hb); hasN(8,hc); hasN(8,hd); hasN(8,he)
    append(8,sc); append(8,sd);   has(8,hc); hasN(8,hb); has(8,hc); has(8,hd); hasN(8,he)

    has(10,hc); hasN(10,hb); has(10,hc); has(10,hd); hasN(10,he)
    hasN(11,hc); hasN(11,hb); hasN(11,hc); hasN(11,hd); hasN(11,he)

  }

  test("time diamond x2 neg") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TimeWindow(2), Diamond, pX),
      jX <= gX and WindowAtom(TimeWindow(4), Diamond, sX),
      hX <= iX not jX
    ) ++ guards
    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); has(1,hb)
    append(1,sb); hasN(1,hb)
    hasN(2,hb)
    hasN(3,hb)
    hasN(4,hb)
    append(4,pb); hasN(4,hb)
    hasN(5,hb)
    has(6,hb)

    append(8,pc); hasN(8,hb); has(8,hc)
    append(8,sc); hasN(8,hb); hasN(8,hc)

    hasN(10,hc); hasN(10,hb);
    hasN(11,hc); hasN(11,hb);

  }

  test("time box x2") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TimeWindow(2), Box, pX),
      jX <= gX and WindowAtom(TimeWindow(4), Box, sX),
      hX <= iX and jX
    ) ++ guards
    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(1,sb)
    append(2,sb)
    append(3,pb); append(3,sb)
    append(4,pb); append(4,sb);
    append(5,pb); append(5,sb); has(5,hb)
    append(6,pb); hasN(6,hb)
    append(6,sb); has(6,hb)
    hasN(7,hb)

  }

  test("time at x2 different time") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TimeWindow(2), At(U1), pX),
      jX <= gX and WindowAtom(TimeWindow(4), At(U2), sX),
      hX <= iX and jX,
      LarsFact(gb)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); hasN(1,hb)
    append(1,sb); has(1,hb)
    has(2,hb)
    has(3,hb)
    hasN(4,hb)
    append(4,pb); has(4,hb)
    has(5,hb)
    hasN(6,hb)

    append(10,pb); append(10,sb)
    has(10,hb)
    has(11,hb)
    append(12,pb)
    has(12,hb)
    has(13,hb)
    has(14,hb)
    hasN(15,hb)

  }

  test("time at x2 same time") {

    val program = LarsProgram.from(
      AtAtom(U,iX) <= gX and WindowAtom(TimeWindow(2), At(U), pX),
      AtAtom(U,jX) <= gX and WindowAtom(TimeWindow(4), At(U), sX),
      //hX <= AtAtom(U,iX) and AtAtom(U,jX), does not work
      hX <= WindowAtom(TimeWindow(2),At(U),iX) and WindowAtom(TimeWindow(4),At(U),jX),
      LarsFact(gb)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); hasN(1,hb)
    append(1,sb); has(1,hb)
    has(2,hb)
    has(3,hb)
    hasN(4,hb)
    append(4,pb); hasN(5,hb)
    hasN(5,hb)
    hasN(6,hb)

    append(10,pb); append(10,sb)
    has(10,hb)
    has(11,hb)
    append(12,pb)
    has(12,hb) //@10!
    hasN(13,hb)
    hasN(14,hb)
    hasN(15,hb)

  }

  test("time at x2 same time, var") {

    val program = LarsProgram.from(
      AtAtom(U,iX) <= gX and WindowAtom(TimeWindow(2), At(U), pX),
      AtAtom(U,jX) <= gX and WindowAtom(TimeWindow(4), At(U), sX),
      AtAtom(U,hX) <= gX and WindowAtom(TimeWindow(2),At(U),iX) and WindowAtom(TimeWindow(4),At(U),jX),
      LarsFact(gb)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); hasN(1,hb)
    append(1,sb); has(1,hb)
    hasN(2,hb) //@1 hb
    hasN(3,hb) //@1 hb
    hasN(4,hb)
    append(4,pb); hasN(5,hb)
    hasN(5,hb)
    hasN(6,hb)

    append(10,pb); append(10,sb)
    has(10,hb) //@10
    hasN(11,hb)
    append(12,pb)
    hasN(12,hb) //@10
    hasN(13,hb)
    hasN(14,hb)
    hasN(15,hb)

  }

  test("time at, plus") {

    pending //current limitation

    val program = LarsProgram.from(
      AtAtom(U,iX) <= gX and WindowAtom(TimeWindow(4), At(U), pX),
      AtAtom(U2,hX) <= gX and WindowAtom(TimeWindow(4), At(U1), iX) and Incr(U1,U2),
      //AtAtom(U2,jX) <= gX and WindowAtom(SlidingTimeWindow(4), At(U1), pX) and Plus(U1,IntValue(1),U2),
      LarsFact(gb)
    )

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); has(1,ib)
    has(2,hb)
    //has(2,jb)

  }

  //

  test("tuple diamond x2") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TupleWindow(4), Diamond, pX),
      jX <= gX and WindowAtom(TupleWindow(4), Diamond, sX),
      hX <= iX and jX
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); append(1,pc); append(1,pd); append(1,pe)
    hasN(1,hb); hasN(1,hc); hasN(1,hd); hasN(1,he)
    append(1,se)
    hasN(1,hb); hasN(1,hc); hasN(1,hd); has(1,he)
    append(1,sd)
    hasN(1,hb); hasN(1,hc); has(1,hd); has(1,he)
    hasN(2,hb); hasN(2,hc); has(2,hd); has(2,he)
    hasN(3,hb); hasN(3,hc); has(3,hd); has(3,he)

    append(4,pe)
    hasN(4,hb); hasN(4,hc); hasN(4,hd); has(4,he)
    hasN(5,hb); hasN(5,hc); hasN(5,hd); has(5,he)

    append(6,pd)
    hasN(6,hb); hasN(6,hc); has(6,hd); has(6,he)

    append(7,pb); append(7,pc)
    hasN(7,hb); hasN(7,hc); hasN(7,hd); hasN(7,he)

    append(8,sd)
    hasN(8,hb); hasN(8,hc); has(8,hd); hasN(8,he)

    append(9,sb)
    has(9,hb); hasN(9,hc); hasN(9,hd); hasN(9,he)

    append(10,sc)
    hasN(10,hb); has(10,hc); hasN(10,hd); hasN(10,he)

    append(11,pe); append(11,se)
    hasN(11,hb); hasN(11,hc); hasN(11,hd); has(11,he)
    hasN(15,hb); hasN(15,hc); hasN(15,hd); has(15,he)

    append(20,pb); append(20,sb)
    has(20,hb); hasN(20,hc); hasN(20,hd); has(20,he)

    append(21,pc); append(21,pd); append(21,pe); append(21,pf)
    hasN(21,hb); hasN(21,hc); hasN(21,hd); hasN(21,he); hasN(21,hf)

  }

  test("tuple at x2 different time") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TupleWindow(4), At(U), pX),
      jX <= gX and WindowAtom(TupleWindow(4), At(U), sX),
      hX <= iX and jX
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); append(1,pc); append(1,pd); append(1,pe)
    hasN(1,hb); hasN(1,hc); hasN(1,hd); hasN(1,he)
    append(1,se)
    hasN(1,hb); hasN(1,hc); hasN(1,hd); has(1,he)
    append(1,sd)
    hasN(1,hb); hasN(1,hc); has(1,hd); has(1,he)
    hasN(2,hb); hasN(2,hc); has(2,hd); has(2,he)
    hasN(3,hb); hasN(3,hc); has(3,hd); has(3,he)

    append(4,pe)
    hasN(4,hb); hasN(4,hc); hasN(4,hd); has(4,he)
    hasN(5,hb); hasN(5,hc); hasN(5,hd); has(5,he)

    append(6,pd)
    hasN(6,hb); hasN(6,hc); has(6,hd); has(6,he)

    append(7,pb); append(7,pc)
    hasN(7,hb); hasN(7,hc); hasN(7,hd); hasN(7,he)

    append(8,sd)
    hasN(8,hb); hasN(8,hc); has(8,hd); hasN(8,he)

    append(9,sb)
    has(9,hb); hasN(9,hc); hasN(9,hd); hasN(9,he)

    append(10,sc)
    hasN(10,hb); has(10,hc); hasN(10,hd); hasN(10,he)

    append(11,pe); append(11,se)
    hasN(11,hb); hasN(11,hc); hasN(11,hd); has(11,he)
    hasN(15,hb); hasN(15,hc); hasN(15,hd); has(15,he)

    append(20,pb); append(20,sb)
    has(20,hb); hasN(20,hc); hasN(20,hd); has(20,he)

    append(21,pc); append(21,pd); append(21,pe); append(21,pf)
    hasN(21,hb); hasN(21,hc); hasN(21,hd); hasN(21,he); hasN(21,hf)

  }

  test("tuple at x2 same time") {

    val program = LarsProgram.from(
      AtAtom(U,iX) <= gX and WindowAtom(TupleWindow(4), At(U), pX),
      AtAtom(U,jX) <= gX and WindowAtom(TupleWindow(4), At(U), sX),
      //hX <= AtAtom(U,iX) and AtAtom(U,jX), does not work.
      //hence trick/hack (note that tuple window is not available on intensional atoms!)
      hX <= gX and WindowAtom(TimeWindow(100), At(U), iX) and WindowAtom(TimeWindow(100), At(U), jX)
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); append(1,pc); append(1,pd); append(1,pe)
    hasN(1,hb); hasN(1,hc); hasN(1,hd); hasN(1,he)
    append(1,se)
    hasN(1,hb); hasN(1,hc); hasN(1,hd); has(1,he)
    append(1,sd)
    hasN(1,hb); hasN(1,hc); has(1,hd); has(1,he)
    hasN(2,hb); hasN(2,hc); has(2,hd); has(2,he)
    hasN(3,hb); hasN(3,hc); has(3,hd); has(3,he)

    append(4,pe)
    hasN(4,hb); hasN(4,hc); hasN(4,hd); has(4,he)
    hasN(5,hb); hasN(5,hc); hasN(5,hd); has(5,he)

    append(6,pd)
    hasN(6,hb); hasN(6,hc); hasN(6,hd); hasN(6,he)

    append(7,pb); append(7,pc)
    hasN(7,hb); hasN(7,hc); hasN(7,hd); hasN(7,he)

    append(8,sd)
    hasN(8,hb); hasN(8,hc); hasN(8,hd); hasN(8,he)

    append(9,sb)
    hasN(9,hb); hasN(9,hc); hasN(9,hd); hasN(9,he)

    append(10,sc)
    hasN(10,hb); hasN(10,hc); hasN(10,hd); hasN(10,he)

    append(11,pe); append(11,se)
    hasN(11,hb); hasN(11,hc); hasN(11,hd); has(11,he)
    hasN(15,hb); hasN(15,hc); hasN(15,hd); has(15,he)

    append(20,pb); append(20,sb)
    has(20,hb); hasN(20,hc); hasN(20,hd); has(20,he)

    append(21,pc); append(21,pd); append(21,pe); append(21,pf)
    hasN(21,hb); hasN(21,hc); hasN(21,hd); hasN(21,he); hasN(21,hf)

  }

  test("tuple box - size 2 neg") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TupleWindow(2), Box, pX),
      jX <= gX and WindowAtom(TupleWindow(2), Box, sX),
      hX <= iX not jX
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(3,pb); has(3,pb); hasN(3,hb)
    empty(4)
    empty(5)
    empty(6)
    append(8,pb); has(8,pb); hasN(8,hb) //#1
    append(9,pb); has(9,pb); has(9,hb) //#2
    append(10,pb); has(10,pb); has(10,hb) //#3
    append(11,pb); has(11,pb); has(11,hb) //#4
    append(11,pc); has(11,hb)//#5
    append(11,pd); has(11,pb); hasN(11,hb) //#6
    append(11,pe); hasN(11,hb) //#7
    empty(12)
    empty(13)
    append(15,pc); hasN(15,hb)
    append(15,pb); has(15,hb)
    append(16,pb); has(16,hb)
    append(17,pb); has(17,hb)
    append(17,pc); has(17,hb)
    append(18,pb); hasN(18,hb)
    append(19,pb); has(19,hb)
    hasN(20,hb)

  }

  test("tuple box - size 3 neg") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TupleWindow(3), Box, pX),
      jX <= gX and WindowAtom(TupleWindow(3), Box, sX),
      hX <= iX not jX
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(1,pb); has(1,pb); hasN(1,hb)
    append(2,pb); hasN(2,hb)
    append(3,pb); has(3,hb)
    append(3,pc); has(3,hb)
    append(3,pd); has(3,hb)
    append(3,pe); hasN(3,hb)
    append(4,pb); hasN(4,hb)
    append(5,pb); hasN(5,hb)
    append(5,pc); has(5,hb)
    append(6,pb); has(6,hb)
    append(7,pb); hasN(7,hb)
    append(8,pb); has(8,hb)
    hasN(9,hb)

  }

  test("tuple box - size 4 neg") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TupleWindow(4), Box, pX),
      jX <= gX and WindowAtom(TupleWindow(4), Box, sX),
      hX <= iX not jX
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)
    def yes(t: Long) = containsWithReasoner(reasoner)(t,hb)
    def no(t: Long) = notContainsWithReasoner(reasoner)(t,hb)

    no(1)
    append(1,pc); no(1)
    append(1,pd); no(1)
    append(1,pe); no(1)
    append(1,pb); yes(1)

    append(2,pb); yes(2)
    append(2,pc); yes(2)
    append(2,pd); yes(2)
    append(2,pe); yes(2)
    append(2,pf); no(2)

    no(3)
    append(3,pc); no(3)
    append(3,pd); no(3)
    append(3,pe); no(3)
    append(3,pb); yes(3)

    append(4,pc); no(4)
    append(4,pd); no(4)
    append(4,pb); yes(4)
    append(4,pe); yes(4)

    append(5,pb); yes(5)

    append(6,pb); yes(6)

    append(7,pb); no(7)

    append(8,pb); yes(8)
    append(8,pc); yes(8)
    append(8,pd); yes(8)
    append(8,pe); yes(8)

    append(9,pb); no(9)
    append(9,pc); no(9)
    append(9,pd); no(9)
    append(9,pe); yes(9)

    append(10,pc); no(10)
    append(10,pd); no(10)
    append(10,pe); no(10)
    append(10,pb); yes(10)

    append(11,pb); yes(11)

    append(12,pb); yes(12)

    append(13,pb); yes(13)

    append(14,pb); yes(14)

    no(15)

  }

  test("tuple box - size 4 pos") {

    val program = LarsProgram.from(
      iX <= gX and WindowAtom(TupleWindow(4), Box, pX),
      jX <= gX and WindowAtom(TupleWindow(4), Box, sX),
      hX <= iX and jX
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)
    def yes(t: Long) = containsWithReasoner(reasoner)(t,hb)
    def no(t: Long) = notContainsWithReasoner(reasoner)(t,hb)

    no(1)
    append(1,pc); no(1)
    append(1,pd); no(1)
    append(1,pe); no(1)
    append(1,pb); no(1)
    append(1,sb); yes(1)

    append(2,pb); no(2)
    append(2,sb); yes(2)
    append(2,pe); no(2)

    no(3)
    append(3,pb); no(3)
    append(3,sb); no(3)

    no(4)
    append(4,pb); no(4)
    append(4,sb); yes(4)
    append(4,pe); no(4)
    append(4,pf); yes(4)

    no(5)
    append(5,pf); no(5)
    append(5,pe); no(5)
    append(5,pb); no(5)
    append(5,sb); yes(5)

    append(6,pb); no(6)
    append(6,sb); yes(6)

    append(7,sb); no(7)
    append(7,pb); yes(7)
    append(7,pe); no(7)
    append(7,pf); yes(7)

    no(8)

  }

  test("at-abstraction time diamond") {
    // tests from: "time at x2 different time"

    val program = LarsProgram.from(
      AtAtom(U,iX) <= gX and WindowAtom(TimeWindow(2), At(U), pX),
      AtAtom(U,jX) <= gX and WindowAtom(TimeWindow(4), At(U), sX),
      hX <= WindowAtom(TimeWindow(2),Diamond,iX) and WindowAtom(TimeWindow(4),Diamond,jX)
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); hasN(1,hb)
    append(1,sb); has(1,hb)
    has(2,hb)
    has(3,hb)
    hasN(4,hb)
    append(4,pb); has(4,hb)
    has(5,hb)
    hasN(6,hb)

    append(10,pb); append(10,sb)
    has(10,hb)
    has(11,hb)
    append(12,pb)
    has(12,hb)
    has(13,hb)
    has(14,hb)
    hasN(15,hb)

  }

  test("at-abstraction time box") {
    //tests from "time box x2"

    val program = LarsProgram.from(
      AtAtom(U,iX) <= gX and WindowAtom(TimeWindow(2), At(U), pX),
      AtAtom(U,jX) <= gX and WindowAtom(TimeWindow(4), At(U), sX),
      hX <= WindowAtom(TimeWindow(2),Box,iX) and WindowAtom(TimeWindow(4),Box,jX)
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    append(1,sb)
    append(2,sb)
    append(3,pb); append(3,sb)
    append(4,pb); append(4,sb);
    append(5,pb); append(5,sb); has(5,hb)
    append(6,pb); hasN(6,hb)
    append(6,sb); has(6,hb)
    hasN(7,hb)

  }

  test("at-abstraction tuple diamond") {
    //tests from "tuple diamond x2"

    val program = LarsProgram.from(
      AtAtom(U,iX) <= gX and WindowAtom(TupleWindow(4), At(U), pX),
      AtAtom(U,jX)<= gX and WindowAtom(TupleWindow(4), At(U), sX),
      hX <= WindowAtom(TimeWindow(100),Diamond,iX) and WindowAtom(TimeWindow(100),Diamond,jX) //se comments above
    ) ++ guards

    val reasoner = reasonerBuilder(program)
    def has = containsWithReasoner(reasoner) _
    def hasN = notContainsWithReasoner(reasoner) _
    def empty = emptyInReasoner(reasoner) _
    def append(t: Long, atom: Atom) = reasoner.append(t)(atom)

    empty(0)

    append(1,pb); append(1,pc); append(1,pd); append(1,pe)
    hasN(1,hb); hasN(1,hc); hasN(1,hd); hasN(1,he)
    append(1,se)
    hasN(1,hb); hasN(1,hc); hasN(1,hd); has(1,he)
    append(1,sd)
    hasN(1,hb); hasN(1,hc); has(1,hd); has(1,he)
    hasN(2,hb); hasN(2,hc); has(2,hd); has(2,he)
    hasN(3,hb); hasN(3,hc); has(3,hd); has(3,he)

    append(4,pe)
    hasN(4,hb); hasN(4,hc); hasN(4,hd); has(4,he)
    hasN(5,hb); hasN(5,hc); hasN(5,hd); has(5,he)

    append(6,pd)
    hasN(6,hb); hasN(6,hc); has(6,hd); has(6,he)

    append(7,pb); append(7,pc)
    hasN(7,hb); hasN(7,hc); hasN(7,hd); hasN(7,he)

    append(8,sd)
    hasN(8,hb); hasN(8,hc); has(8,hd); hasN(8,he)

    append(9,sb)
    has(9,hb); hasN(9,hc); hasN(9,hd); hasN(9,he)

    append(10,sc)
    hasN(10,hb); has(10,hc); hasN(10,hd); hasN(10,he)

    append(11,pe); append(11,se)
    hasN(11,hb); hasN(11,hc); hasN(11,hd); has(11,he)
    hasN(15,hb); hasN(15,hc); hasN(15,hd); has(15,he)

    append(20,pb); append(20,sb)
    has(20,hb); hasN(20,hc); hasN(20,hd); has(20,he)

    append(21,pc); append(21,pd); append(21,pe); append(21,pf)
    hasN(21,hb); hasN(21,hc); hasN(21,hd); hasN(21,he); hasN(21,hf)

  }

}
