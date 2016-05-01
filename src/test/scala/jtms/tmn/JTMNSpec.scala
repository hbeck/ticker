package jtms.tmn

import core.{AspFact, AspRule, UserDefinedAtom, Atom}
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMNSpec extends FlatSpec {
  def Assumption(node: String): AspRule = Assumption(UserDefinedAtom(node))

  def Assumption(node: Atom): AspRule = AspFact(node)

  def EmptyTMN = new JTMNRefactored()
}
