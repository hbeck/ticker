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

  val j1 = Rule.pos(c).head(a)
  val j2 = Rule.neg(a).head(b)
  val j3 = Rule.pos(a).head(c)
  val j4a = Rule.pos(b).head(d)
  val j4b = Rule.pos(c).head(d)
  val j5 = Fact(e)
  val j6 = Rule.pos(c, e).head(f)

  val program = Program(j1, j2, j3, j4a, j4b, j5, j6)

  def JTMS = {
    val tmn = TMN(program)

    tmn
  }
}
