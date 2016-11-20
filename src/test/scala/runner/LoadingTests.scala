package runner

import java.util.concurrent.TimeUnit

import core.{Atom, Predicate, lars}
import core.lars.{SlidingTimeWindow, SlidingTupleWindow, TimeWindowSize}
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

  "w_d_7_a" should "lead to a parsing error" in {
    intercept[RuntimeException] {
      loader.windowAtom("w_d_7_a", Seq())
    }
  }
}
