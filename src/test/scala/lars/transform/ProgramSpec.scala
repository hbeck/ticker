package lars.transform

import core.asp.AspRule
import core.lars.{LarsFact, LarsProgram}
import engine.asp.PlainLarsToAsp
import org.scalatest.Matchers._

/**
  * Created by FM on 12.05.16.
  */
class ProgramSpec extends TransformLarsSpec {

  "A program with one Fact a." should "be tranformed into one rule" in {
    val p = LarsProgram.from(LarsFact(a))

    PlainLarsToAsp(p).rules should have size 1
  }
  it should "contain a(T)." in {
    val p = LarsProgram.from(LarsFact(a))

    PlainLarsToAsp(p).rules should contain(AspRule(a(T), Set(now(T))))
  }

  "A program with two different Facts a. b." should "be transformed into 2 rules a(T). b(T)." in {
    val p = LarsProgram.from(
      LarsFact(a),
      LarsFact(b)
    )

    PlainLarsToAsp(p).rules should have size 2
  }
}
