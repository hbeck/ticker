package jtms.clingo

import core.{Rule, Premise, Atom}
import org.scalatest.FlatSpec

/**
  * Created by FM on 22.02.16.
  */
class AspSpecs extends FlatSpec {

  val A = Atom("A")
  val B = Atom("B")
  val C = Atom("C")

  "A premise A" should "be transformed into the expression 'A.'" in {

    val premise = Premise(A)

    assert(Asp(premise) == AspExpression("A."))
  }

  "A rule with only positive support" should "be transformed into the expression 'A :- B.'" in {
    val j = Rule.in(B).head(A)

    assert(Asp(j) == AspExpression("A :- B."))
  }
  it should "be transformed into the expression 'A :- B, C.'" in {
    val j = Rule.in(B, C).head(A)

    assert(Asp(j) == AspExpression("A :- B, C."))
  }

  "A rule with only negative support" should "be transformed into the expression 'A :- not B.'" in {
    val j = Rule.out(B).head(A)

    assert(Asp(j) == AspExpression("A :- not B."))
  }
  it should "be transformed into the expression 'A:- not B, not C" in {
    val j = Rule.out(B, C).head(A)

    assert(Asp(j) == AspExpression("A :- not B, not C."))
  }

  "A rule with both positive an negative support" should "be transformed into 'A :- B, not C.'" in {
    val j = Rule.in(B).out(C).head(A)

    assert(Asp(j) == AspExpression("A :- B, not C."))
  }
}
