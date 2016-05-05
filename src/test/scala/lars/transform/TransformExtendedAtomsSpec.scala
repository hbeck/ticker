package lars.transform

import core.Atom
import core.lars.{Diamond, SlidingTimeWindow, WindowAtom, AtAtom}
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

  "The window-atom wˆ1 d a" should "be transformed into w_1_d_a(T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), Diamond, a)
    assert(TransformLars(window) == Atom("w_1_d_a")(T))
  }

}
