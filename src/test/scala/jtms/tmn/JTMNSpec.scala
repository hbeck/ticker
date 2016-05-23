package jtms.tmn

import core.asp.{AspFact, PlainAspRule}
import core.{Atom, UserDefinedAtom}
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMNSpec extends FlatSpec {
  def Assumption(node: String): PlainAspRule = Assumption(UserDefinedAtom(node))

  def Assumption(node: Atom): PlainAspRule = AspFact(node)

  def EmptyTMN = new JTMNRefactored()
}
