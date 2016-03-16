package jtms.tmn.examples

import core._
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMSSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")
  val e = Atom("e")
  val f = Atom("f")

  val none = Set[Atom]()
  //    val j1 = a :- PosBuilderAtom(c), NegBuilderAtom(d), PosBuilderAtom(e)
  //  val j1 = a :- c and d and not(e) and not(f)
  //, Not(d), e)
  val j1 = Rule.pos(c).head(a)
  val j2 = Rule.neg(a).head(b)
  val j3 = Rule.pos(a).head(c)
  val j4a = Rule.pos(b).head(d)
  val j4b = Rule.pos(c).head(d)
  val j5 = Fact(e)
  val j6 = Rule.pos(c, e).head(f)

  val j1 = Rule(a, c)
  val j2 = Rule(b, none, Set(a))
  val j3 = Rule(c, a)
  val j4a = Rule(d, b)
  val j4b = Rule(d, c)
  val j5 = Rule(e)
  val j6 = Rule(f, Set(c, e))

  val program = Program(j1, j2, j3, j4a, j4b, j5, j6)

  val program2 = ProgramBuilder(j2)(j3)(j4a)
  val program3 = ProgramBuilder rule j1 rule j2
  //    .rule(j3)
  //  j4a
  //  j4b
  //  j5
  //  j6)

  val program4 = ProgramBuilder
    .rule(a :- c and d and not(e) and not(f))
    .rule(:- ( d) and not( e) )
  //    .rule(j3)
  //  j4a
  //  j4b
  //  j5
  //  j6)


  def JTMS = {
    val tmn = TMN(program3.toProgram)

    tmn
  }
}
