package lars

import core.Atom
import core.lars._
import engine.{TransformLars, Time}
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.05.16.
  */
class TransformLarsSpec extends FlatSpec {
  val t1 = Time(1)
  val t2 = Time(2)

  val T = TransformLars.T

  val a = Atom("a")
  val b = Atom("b")

  "An atom a" should "be transformed into a(T)" in {
    assert(TransformLars(a) == a(T))
  }
  "An atom a(1)" should "be transformed into a(1,T)" in {
    assert(TransformLars(a("1")) == a("1", T))
  }

  "An at-atom @_t1 a" should "be transformed into a(t1)" in {
    assert(TransformLars(AtAtom(t1, a)) == a(t1.toString))
  }
  "An at-atom @_t1 a(1)" should "be transformed into a(1,t1)" in {
    assert(TransformLars(AtAtom(t1, a("1"))) == a("1", t1.toString))
  }

  "The window-atom wˆ1 d a" should "be transformed into w_1_d_a(T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), Diamond, a)
    assert(TransformLars(window) == Atom("w_1_d_a")(T))
  }

  "The name for window-atom wˆ1 d a" should "be w_1_d_a" in {
    val window = WindowAtom(SlidingTimeWindow(1), Diamond, a)
    assert(TransformLars.nameFor(window) == "w_1_d_a")
  }
  "The name for window-atom wˆ1 b a" should "be w_1_b_a" in {
    val window = WindowAtom(SlidingTimeWindow(1), Box, a)
    assert(TransformLars.nameFor(window) == "w_1_b_a")
  }
  "The name for window-atom wˆ1 at_1 a" should "be w_1_at_1_a" in {
    val window = WindowAtom(SlidingTimeWindow(1), At(t1), a)
    assert(TransformLars.nameFor(window) == "w_1_at_1_a")
  }
  "The name for window-atom wˆ1 at_2 a" should "be w_1_at_2_a" in {
    val window = WindowAtom(SlidingTimeWindow(1), At(t2), a)
    assert(TransformLars.nameFor(window) == "w_1_at_2_a")
  }
  "The name for window-atom wˆ1 d b" should "be w_1_d_b" in {
    val window = WindowAtom(SlidingTimeWindow(1), Diamond, b)
    assert(TransformLars.nameFor(window) == "w_1_d_b")
  }
  "The name for window-atom wˆ2 d b" should "be w_2_d_b" in {
    val window = WindowAtom(SlidingTimeWindow(2), Diamond, b)
    assert(TransformLars.nameFor(window) == "w_2_d_b")
  }
}
