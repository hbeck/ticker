package jtms.tmn.examples

import core.{Rule, Atom}
import jtms.TMN
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
class MultipleModels extends FlatSpec{

  val A = Atom("A")
  val B = Atom("B")

  val none = Set[Atom]()

  val rB = Rule(B,none,Set(A))
  val rA = Rule(A,none,Set(B))

  "When adding rB before rA the valid model" should "be B" in {
    val tmn = TMN()

    tmn.add(rB)
    tmn.add(rA)

    assert(tmn.model().get == Set(B))
  }

  "When adding rA before rB the valid model" should "be A" in {
    val tmn = TMN()

    tmn.add(rA)
    tmn.add(rB)

    assert(tmn.model().get == Set(A))

  }
}
