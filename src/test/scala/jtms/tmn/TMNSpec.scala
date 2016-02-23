package jtms.tmn

import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class TMNSpec extends FlatSpec {
  def Assumption(node: String): Justification = Assumption(UserDefinedNode(node))

  def Assumption(node: Node): Justification = Premise(node)

  def EmptyTMN = new TMN(Set())
}
