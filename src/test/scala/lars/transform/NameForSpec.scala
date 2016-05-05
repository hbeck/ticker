package lars.transform

import core.lars._
import engine.TransformLars

/**
  * Created by FM on 05.05.16.
  */
class NameForSpec extends TransformLarsSpec {
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
