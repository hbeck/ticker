package jtms.tmn.examples

import core.{Rule, Atom}
import jtms.TMN
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
class MultipleModels  extends  FlatSpec{

  val A = Atom("A")
  val B = Atom("B")

  val j1 = Rule.neg(A).head(B)
  val j2 = Rule.neg(B).head(A)

  def TMN0 = TMN(Set(A,B))

  "When adding j1 before j2 the valid model" should "be B" in{
    val tmn = TMN0

    tmn.add(j1)
    tmn.add(j2)

    assert(tmn.getModel() == Set(B))
    pending
  }

  "When adding j2 before j1 the valid model" should "be A" in{
    val tmn = TMN0

    tmn.add(j2)
    tmn.add(j1)

    assert(tmn.getModel() == Set(A))
    pending
  }
}
