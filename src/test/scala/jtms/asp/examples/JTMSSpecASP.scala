package jtms.asp.examples

import core._
import core.asp.{AspFact, AspProgram, AspRule}
import fixtures.AtomTestFixture
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16. / HB on 31.03.16
  */
class JTMSSpecASP extends FlatSpec with AtomTestFixture {
  val none = Set[Atom]()

  val j1 = AspRule(a, c)
  val j2 = AspRule(b, none, Set(a))
  val j3 = AspRule(c, a)
  val j4a = AspRule(d, b)
  val j4b = AspRule(d, c)
  val j5 = AspFact(e)
  val j6 = AspRule(f, Set(c, e))

  val program = AspProgram(j1, j2, j3, j4a, j4b, j5, j6)
  //val program = Program(j5, j3, j1, j2, j4a, j4b, j6)

  def Network = ExtendedJtms(program)

}

