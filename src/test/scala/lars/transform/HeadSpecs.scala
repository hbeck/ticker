package lars.transform

import core.Atom
import core.lars.{Box, Diamond, WindowAtom}
import engine.asp.LarsToPinnedProgram
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class HeadSpecs extends TransformLarsSpec {

  val w_te_1_d_a = WindowAtom(st1, Diamond, a)
  "The head for wˆ1 d a" should "be the Atom (w_te_1_d_a)" in {
    DefaultLarsToPinnedProgram.head(w_te_1_d_a).toString should include("w_te_1_d_a")
  }
  it should "have arity 1" in {
    DefaultLarsToPinnedProgram.head(w_te_1_d_a).arity should be(1)
  }
  it should "have TimeVariable T as argument" in {
    DefaultLarsToPinnedProgram.head(w_te_1_d_a).toString should include("T")
  }

  "The head for wˆ1 b a" should "be w_te_1_b_a(T)" in {
    DefaultLarsToPinnedProgram.head(WindowAtom(st1, Box, a)) should equal(Atom("w_te_1_b_a")(T))
  }
}
