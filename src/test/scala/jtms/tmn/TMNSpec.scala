package jtms.tmn

import core.{Fact, Rule, UserDefinedAtom, Atom}
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class TMNSpec extends FlatSpec {
  def Assumption(node: String): Rule = Assumption(UserDefinedAtom(node))

  def Assumption(node: Atom): Rule = Fact(node)

  def EmptyTMN = new TMNRefactored()
}
