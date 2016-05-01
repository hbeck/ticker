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

  val j1 = AspRule(a, c)
  val j2 = AspRule(b, none, Set(a))
  val j3 = AspRule(c, a)
  val j4a = AspRule(d, b)
  val j4b = AspRule(d, c)
  val j5 = AspRule(e)
  val j6 = AspRule(f, Set(c, e))

  val program = AspProgram(j1, j2, j3, j4a, j4b, j5, j6)


  def JTMS = {
    val tmn = JTMNRefactored(program)

    tmn
  }
}

