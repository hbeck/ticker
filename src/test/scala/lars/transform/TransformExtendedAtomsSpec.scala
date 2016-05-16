package lars.transform

import core.Atom
import core.lars._
import engine.asp.PlainLarsToAsp

/**
  * Created by FM on 05.05.16.
  */
class TransformExtendedAtomsSpec extends TransformLarsSpec {

  "An atom a" should "be transformed into a(T)" in {
    assert(PlainLarsToAsp(a) == a(T))
  }
  "An atom a(1)" should "be transformed into a(1,T)" in {
    assert(PlainLarsToAsp(a("1")) == a("1", T.variable))
  }

  "An at-atom @_t1 a" should "be transformed into a(t1)" in {
    assert(PlainLarsToAsp(AtAtom(t1, a)) == a(t1))
  }
  "An at-atom @_t1 a(1)" should "be transformed into a(1,t1)" in {
    assert(PlainLarsToAsp(AtAtom(t1, a("1"))) == a("1", t1.toString))
  }

  "The window-atom wˆ1 d a" should "be transformed into w_1_d_a(T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), Diamond, a)
    assert(PlainLarsToAsp(window) == Atom("w_1_d_a")(T))
  }
  "The window-atom wˆ1 b a(1)" should "be transformed into w_1_b_a(1,T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), Box, a("1"))
    assert(PlainLarsToAsp(window) == Atom("w_1_b_a")("1", T.variable))
  }
  "The window-atom wˆ1 at_1 a" should "be transformed into w_1_at_1_a(T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), At(t1), a)
    assert(PlainLarsToAsp(window) == Atom("w_1_at_1_a")(T))
  }
  "The window-atom wˆ1 at_U a" should "be transformed into w_1_at_U_a(U, T)" in {
    val U = TimeVariable("U")
    val window = WindowAtom(SlidingTimeWindow(1), At(U), a)
    assert(PlainLarsToAsp(window) == Atom("w_1_at_U_a")(U.toString,T.toString))
  }

}
