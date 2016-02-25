package jtms.tmn

import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class TMNSpec extends FlatSpec {
  def Assumption(node: String): Justification = Assumption(UserDefinedAtom(node))

  def Assumption(node: Atom): Justification = Premise(node)

  def EmptyTMN = new TMN(Set())
}
