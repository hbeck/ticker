package asp

import core.{Program, Rule, Fact, Atom}
import org.scalatest.FlatSpec

/**
  * Created by FM on 22.02.16.
  */
class AspConversionSpecs extends FlatSpec {

  val A = Atom("A")
  val B = Atom("B")
  val C = Atom("C")

  "A premise A" should "be transformed into the expression 'A.'" in {
    val premise = Fact(A)

    assert(AspConversion(premise) == AspExpression("A."))
  }

  "A rule with only positive support" should "be transformed into the expression 'A :- B.'" in {
    val j = Rule.pos(B).head(A)

    assert(AspConversion(j) == AspExpression("A :- B."))
  }
  it should "be transformed into the expression 'A :- B, C.'" in {
    val j = Rule.pos(B, C).head(A)

    assert(AspConversion(j) == AspExpression("A :- B, C."))
  }

  "A rule with only negative support" should "be transformed into the expression 'A :- not B.'" in {
    val j = Rule.neg(B).head(A)

    assert(AspConversion(j) == AspExpression("A :- not B."))
  }
  it should "be transformed into the expression 'A:- not B, not C" in {
    val j = Rule.neg(B, C).head(A)

    assert(AspConversion(j) == AspExpression("A :- not B, not C."))
  }

  "A rule with both positive an negative support" should "be transformed into 'A :- B, not C.'" in {
    val j = Rule.pos(B).neg(C).head(A)

    assert(AspConversion(j) == AspExpression("A :- B, not C."))
  }

  "An empty program" should "return no AspExpressions" in {
    val p = Program()

    assert(AspConversion(p).isEmpty)
  }

  "A program containing one rule" should "return one expression" in {
    val p = Program(Fact(A))

    assert(AspConversion(p).size == 1)
  }
}
