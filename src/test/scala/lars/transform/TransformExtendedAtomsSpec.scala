package lars.transform

import core.Atom
import core.lars._
import engine.TransformLars

/**
  * Created by FM on 05.05.16.
  */
class TransformExtendedAtomsSpec extends TransformLarsSpec {

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

  // TODO: decide if two window-atoms with the same parameter should/could be mapped to the same ASP-Rule
  "The window-atom wˆ1 d a" should "be transformed into w_1_d_a(T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), Diamond, a)
    assert(TransformLars(window) == Atom("w_1_d_a")(T))
  }
  "The window-atom wˆ1 b a(1)" should "be transformed into w_1_b_a(1,T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), Box, a("1"))
    assert(TransformLars(window) == Atom("w_1_b_a")("1", T))
  }

}
