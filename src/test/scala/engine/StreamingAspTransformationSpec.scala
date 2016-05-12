package engine

import core.Atom
import core.lars.AtTime
import engine.implementations.{StreamingAspEvaluation, StreamingAspEvaluation$}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 22.04.16.
  */
class StreamingAspTransformationSpec extends FlatSpec {
  val t1 = AtTime.second(1)
  val t2 = AtTime.second(2)
  val t3 = AtTime.second(3)

  val a = Atom("a")

  "No atom and only time t1" should "be translated into the fact 'now(1000)'" in {
    assert(StreamingAspEvaluation.transform(t1, Set()) == Set("now(1000)."))
  }

  it should "be empty with transformAtoms" in {
    assert(StreamingAspEvaluation.transformAtoms(t1, Set()) == Set())
  }
  it should "be empty with evaluation" in {
    assert(StreamingAspEvaluation.transform(Set()) == Set())
  }

  "One atom and time t2" should "be translated into the facts 'now(2000). a(2000).'" in {
    assertResult(Set("now(2000).", "a(2000).")) {
      StreamingAspEvaluation.transform(t2, Set(StreamEntry(t2, Set(a))))
    }
  }
  it should "be translated into the facts 'a(2000).'" in {
    assertResult(Set("a(2000).")) {
      StreamingAspEvaluation.transformAtoms(t2, Set(a))
    }
  }
  it should "be translated into the facts 'a(2000)." in {
    assertResult(Set("a(2000).")) {
      StreamingAspEvaluation.transform(Set(StreamEntry(t2, Set(a))))
    }
  }

  "Two atoms with arity and time t3" should "be translated into the facts 'now(3000). a(3000). b(1,3000)." in {
    val b = Atom("b").apply("1")

    assertResult(Set("now(3000).", "a(3000).", "b(1,3000).")) {
      StreamingAspEvaluation.transform(t3, Set(StreamEntry(t3, Set(a, b))))
    }
  }

  "An empty set of ASP-Expressions" should "return an empty result" in {
    val convert = StreamingAspEvaluation(Set())
    convert.prepare(t1, Set()).get.value should be(empty)
  }

  // TODO: write test that verifies that all atoms have T as argument
}
