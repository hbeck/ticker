package jtms.clingo

import jtms.{Justification, Node, Premise}
import org.scalatest.FlatSpec

/**
  * Created by FM on 22.02.16.
  */
class TmnTransformationSpecs extends FlatSpec {

  val A = Node("A")
  val B = Node("B")
  val C = Node("C")

  "A premise A" should "be transformed into the expression 'A.'" in {

    val premise = Premise(A)

    assert(TmnTransformation(premise) == "A.")
  }

  "A justification with only positive support" should "be transformed into the expression 'A :- B.'" in {
    val j = Justification.in(B).node(A)

    assert(TmnTransformation(j) == "A :- B.")
  }
  it should "be transformed into the expression 'A :- B, C.'" in {
    val j = Justification.in(B, C).node(A)

    assert(TmnTransformation(j) == "A :- B, C.")
  }

  "A justification with only negative support" should "be transformed into the expression 'A :- not B.'" in {
    val j = Justification.out(B).node(A)

    assert(TmnTransformation(j) == "A :- not B.")
  }
  it should "be transformed into the expression 'A:- not B, not C" in {
    val j = Justification.out(B, C).node(A)

    assert(TmnTransformation(j) == "A :- not B, not C.")
  }

  "A justification with both positive an negative support" should "be transfomred into 'A :- B, not C.'" in {
    val j = Justification.in(B).out(C).node(A)

    assert(TmnTransformation(j) == "A :- B, not C.")
  }
}
