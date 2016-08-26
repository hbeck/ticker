package clingo

import core._
import core.asp.{AspFact, AspProgram, AspRule}
import fixtures.AtomTestFixture
import org.scalatest.FlatSpec

/**
  * Created by FM on 22.02.16.
  */
class AspConversionSpecs extends FlatSpec with AtomTestFixture {

  val Falsum = new ContradictionAtom(Predicate("n"))

  "A premise A" should "be transformed into the expression 'a.'" in {
    val premise = AspFact(a)

    assert(ClingoConversion(premise) == "a.")
  }

  "A rule with only positive support" should "be transformed into the expression 'a :- b.'" in {
    val j = AspRule.pos(b).head(a)

    assert(ClingoConversion(j) == "a :- b.")
  }
  it should "be transformed into the expression 'a :- b, c.'" in {
    val j = AspRule.pos(b, c).head(a)

    assert(ClingoConversion(j) == "a :- b, c.")
  }

  "A rule with only negative support" should "be transformed into the expression 'a :- not b.'" in {
    val j = AspRule.neg(b).head(a)

    assert(ClingoConversion(j) == "a :- not b.")
  }
  it should "be transformed into the expression 'a:- not b, not c" in {
    val j = AspRule.neg(b, c).head(a)

    assert(ClingoConversion(j) == "a :- not b, not c.")
  }

  "A rule with both positive an negative support" should "be transformed into 'a :- b, not c.'" in {
    val j = AspRule.pos(b).neg(c).head(a)

    assert(ClingoConversion(j) == "a :- b, not c.")
  }

  "A rule with a ContradictionNode" should "be transformed into an integrity constraint ':- a, not c.'" in {
    val r = AspRule.pos(a).neg(c).head(Falsum)

    assert(ClingoConversion(r) == ":- a, not c.")
  }

  "An empty program" should "return no AspExpressions" in {
    val p = AspProgram()

    assert(ClingoConversion(p).isEmpty)
  }

  "A program containing one rule" should "return one expression" in {
    val p = AspProgram(AspFact(a))

    assert(ClingoConversion(p).size == 1)
  }

  "An exception" should "be thrown if the Atom starts with an uppercase letter" in {
    val A = Atom("A")

    intercept[IllegalArgumentException] {
      ClingoConversion(AspFact(A))
    }
  }
  it should "be thrown if the Atom contains whitespace characters" in {
    val a_b = Atom("a b")
    intercept[IllegalArgumentException] {
      ClingoConversion(a_b)
    }
  }
  it should "be thrown if the Atom contains non ASCII characters" in {
    val umlaut = Atom("ü")
    intercept[IllegalArgumentException] {
      ClingoConversion(umlaut)
    }
  }

  "An atom a with arity 1" should "be converted into a(1)" in {
    val atomValue = a("1")

    assert(ClingoConversion(atomValue) == "a(1)")
  }
  "An atom a with arity 2" should "be converted into a(asdf,2)" in {
    val atomValue = a("asdf", "2")

    assert(ClingoConversion(atomValue) == "a(asdf,2)")
  }
}
