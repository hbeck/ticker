package lars.transform

import core.Atom
import core.lars.{Box, Diamond, WindowAtom}
import engine.asp.PlainLarsToAsp
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class HeadSpecs extends TransformLarsSpec {

  val w_1_d_a = WindowAtom(st1, Diamond, a)
  "The head for wˆ1 d a" should "be the Atom (w_1_d_a)" in {
    PlainLarsToAsp.head(w_1_d_a).toString should include("w_1_d_a")
  }
  it should "have arity 1" in {
    PlainLarsToAsp.head(w_1_d_a).arity should be(1)
  }
  it should "have TimeVariable T as argument" in {
    PlainLarsToAsp.head(w_1_d_a).toString should include("T")
  }

  "The head for wˆ1 b a" should "be w_1_b_a(T)" in {
    PlainLarsToAsp.head(WindowAtom(st1, Box, a)) should equal(Atom("w_1_b_a")(T))
  }
}
