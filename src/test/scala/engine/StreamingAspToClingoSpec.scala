package engine

import core.Atom
import core.lars.{AtTime, TimePoint}
import engine.asp.evaluation.{StreamingAspToClingo, StreamingClingoInterpreter}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 22.04.16.
  */
class StreamingAspToClingoSpec extends FlatSpec {
  val t1 = TimePoint(1)
  val t2 = TimePoint(2)
  val t3 = TimePoint(3)

  val a = Atom("a")

  "No atom and only time t1" should "be translated into the fact 'now(1)'" in {
    assert(StreamingAspToClingo(t1, Set()) == Set("now(1)."))
  }

  "One atom and time t2" should "be translated into the facts 'now(2). a(2).'" in {
    assertResult(Set("now(2).", "a(2).")) {
      StreamingAspToClingo(t2, Set(StreamEntry(t2, Set(a))))
    }
  }

  "Two atoms with arity and time t3" should "be translated into the facts 'now(3). a(3). b(1,3)." in {
    val b = Atom("b").apply("1")

    assertResult(Set("now(3).", "a(3).", "b(1,3).")) {
      StreamingAspToClingo(t3, Set(StreamEntry(t3, Set(a, b))))
    }
  }

  "An empty set of ASP-Expressions" should "return an empty result" in {
    val convert = StreamingClingoInterpreter(Set())
    convert(Set()) should be(empty)
  }

  // TODO: write test that verifies that all atoms have T as argument
}
