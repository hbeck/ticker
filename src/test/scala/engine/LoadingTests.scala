package engine

import java.util.concurrent.TimeUnit

import core._
import core.lars._
import org.scalatest.FlatSpec
import org.scalatest.matchers
import org.scalatest.Matchers._

/**
  * Created by FM on 19.11.16.
  */
class LoadingTests extends FlatSpec {

  val loader = Load()

  "w_7_d_a(foo,X)" should "say Time Window with size 7 (engine-time-units) Diamond a(foo,X)" in {
    val result = loader.windowAtom("w_7_d_a", Seq("foo", "X"))

    result.atom.predicate should be(Predicate("a"))
    result.windowFunction should be(SlidingTimeWindow(TimeWindowSize(7, TimeUnit.SECONDS)))
  }

  "w_7min_d_a(foo)" should "say Time Window with size 7 minutes Diamond a(foo)" in {
    val result = loader.windowAtom("w_7min_d_a", Seq("foo"))

    result.atom.predicate should be(Predicate("a"))
    result.windowFunction should be(SlidingTimeWindow(TimeWindowSize(7, TimeUnit.MINUTES)))
  }

  "w_7t_d_a" should "say Tuple-Based Window with size 7 Diamond a" in {
    val result = loader.windowAtom("w_7t_d_a", Seq())

    result.atom.predicate should be(Predicate("a"))
    result.windowFunction should be(SlidingTupleWindow(7))
  }

  "w_5t_b_y(1)" should "be parsed into predicate y  with argument 1" in {
    val result = loader.xatom("w_5t_b_y(1)")

    val atom = result.atom.asInstanceOf[AtomWithArguments]

    atom.predicate.caption should be("y")
    atom.arguments should (have size (1) and contain(IntValue(1)))
  }

  it should "also be parsed by xatom" in {
    val result = loader.xatom("w_7t_d_a")

    result.atom.predicate should be(Predicate("a"))
    val window = result.asInstanceOf[WindowAtom]
    window.windowFunction should be(SlidingTupleWindow(7))
    window.temporalModality should be(lars.Diamond)
  }

  "w_d_7_a" should "lead to a parsing error" in {
    intercept[RuntimeException] {
      loader.windowAtom("w_d_7_a", Seq())
    }
  }

  "A program" should "be loadable" in {
    val stringProgram =
      """a :- w_10s_d_x
         b :- w_5t_b_y(1)
         c :- a, not b
      """.stripMargin
    val program = loader.readProgram(stringProgram.lines.toArray)

    program.rules should have length (3)
    program.extendedAtoms.map(_.atom.predicate.caption) should contain allOf("a", "b", "c", "x", "y")
    program.extendedAtoms.map(_.atom) should contain(Atom(Predicate("y"), Seq(IntValue(1))))
  }
}
