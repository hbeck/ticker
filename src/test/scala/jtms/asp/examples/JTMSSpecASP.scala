package jtms.asp.examples

import core._
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16. / HB on 31.03.16
  */
class JTMSSpecASP extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")
  val e = Atom("e")
  val f = Atom("f")

  val none = Set[Atom]()

  val j1 = Rule(a, c)
  val j2 = Rule(b, none, Set(a))
  val j3 = Rule(c, a)
  val j4a = Rule(d, b)
  val j4b = Rule(d, c)
  val j5 = Rule(e)
  val j6 = Rule(f, Set(c, e))

  val program = AspProgram(j1, j2, j3, j4a, j4b, j5, j6)
  //val program = Program(j5, j3, j1, j2, j4a, j4b, j6)

  def Network = ExtendedJTMS(program)

}

