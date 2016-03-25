package jtms.tmn.examples

import core.{Program, Rule, Fact, Atom}
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

  val j1 = Rule(a, c)
  val j2 = Rule(b, none, Set(a))
  val j3 = Rule(c, a)
  val j4a = Rule(d, b)
  val j4b = Rule(d, c)
  val j5 = Rule(e)
  val j6 = Rule(f, Set(c, e))

  val program = Program(j1, j2, j3, j4a, j4b, j5, j6)

  def JTMS = {
    val tmn = JTMNRefactored(program)

    tmn
  }
}
