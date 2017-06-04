package lars.transform

import core.{Atom, PinnedAtom}
import core.lars._

/**
  * Created by FM on 05.05.16.
  */
class TransformExtendedAtomsSpec extends TransformLarsSpec {

  "An atom a" should "be transformed into a(T)" in {
    assert(DefaultLarsToPinnedProgram.encodingAtom(a) == a(T))
  }
  "An atom a(1)" should "be transformed into a_at(1,T)" in {
    assert(DefaultLarsToPinnedProgram.encodingAtom(a("1")) == PinnedAtom.asPinnedAtAtom(a("1"), T))
  }

  "An at-atom @_t1 a" should "be transformed into a(t1)" in {
    assert(DefaultLarsToPinnedProgram.encodingAtom(AtAtom(t1, a)) == a(t1))
  }
  "An at-atom @_t1 a(1)" should "be transformed into a_at(1,t1)" in {
    assert(DefaultLarsToPinnedProgram.encodingAtom(AtAtom(t1, a("1"))) == PinnedAtom.asPinnedAtAtom(a("1"), t1))
  }

  "The window-atom wˆ1 d a" should "be transformed into w_te_1_d_a(T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), Diamond, a)
    assert(DefaultLarsToPinnedProgram.encodingAtom(window) == Atom("w_te_1_d_a")(T))
  }
  "The window-atom wˆ1 b a(1)" should "be transformed into w_te_b_a(1,T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), Box, a("1"))
    assert(DefaultLarsToPinnedProgram.encodingAtom(window) == Atom("w_te_1_b_a")("1", T))
  }
  "The window-atom wˆ1 at_1 a" should "be transformed into w_te_1_at_1_a(T)" in {
    val window = WindowAtom(SlidingTimeWindow(1), At(t1), a)
    assert(DefaultLarsToPinnedProgram.encodingAtom(window) == Atom("w_te_1_at_1_a")(T))
  }
  "The window-atom wˆ1 at_U a" should "be transformed into w_te_1_at_U_a(U, T)" in {
    val U = TimeVariableWithOffset("U")
    val window = WindowAtom(SlidingTimeWindow(1), At(U), a)
    assert(DefaultLarsToPinnedProgram.encodingAtom(window) == Atom("w_te_1_at_U_a")(U)(T))
  }

  "An head-atom with a time-variable as last argument" should "not be transformed" in {
    val U = TimeVariableWithOffset("U")
    val head: HeadAtom = a(U)

    assert(DefaultLarsToPinnedProgram.encodingAtom(head) == head)
  }
  "An head-atom with a computed time-variable as last argument" should "not be transformed" in {
    val U = TimeVariableWithOffset("U")
    val head: HeadAtom = a(U + 1)

    assert(DefaultLarsToPinnedProgram.encodingAtom(head) == head)
  }
}
