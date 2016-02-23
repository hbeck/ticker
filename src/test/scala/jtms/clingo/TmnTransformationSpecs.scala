package jtms.clingo

import jtms.{Justification, Node, Premise}
import org.scalatest.FlatSpec

/**
  * Created by FM on 22.02.16.
  */
class TmnTransformationSpecs extends FlatSpec{

  val A = Node("A")
  val B = Node("B")

  "A premise A" should "be transformed into the expression 'A.'" in {

    val premise = Premise(A)

    assert(TmnTransformation(premise) == "A.")
  }

  "A justification with only positive support" should "be transformed into the expression 'A :- B'" in {
    val j = Justification.in(B).node(A)

    assert(TmnTransformation(j) == "A :- B")
  }

}
