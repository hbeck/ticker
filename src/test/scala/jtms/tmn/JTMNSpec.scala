package jtms.tmn

import core.asp.{AspFact, NormalRule}
import core.{Atom, Predicate}
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMNSpec extends FlatSpec {
  def Assumption(node: String): NormalRule = Assumption(Predicate(node))

  def Assumption(node: Atom): NormalRule = AspFact(node)

  def EmptyTMN = new JtmsRefactored()
}
