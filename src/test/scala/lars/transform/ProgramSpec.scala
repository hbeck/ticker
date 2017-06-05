package lars.transform

import core.asp.{AspFact, AspRule}
import core.lars.{LarsFact, LarsProgram}
import org.scalatest.Matchers._

/**
  * Created by FM on 12.05.16.
  */
class ProgramSpec extends TransformLarsSpec {

  "A program with one Fact a." should "be transformed into one rule" in {
    val p = LarsProgram.from(LarsFact(a))

    DefaultLarsToPinnedProgram(p).rules should have size 1
  }
  it should "contain a." in {
    val p = LarsProgram.from(LarsFact(a))

    DefaultLarsToPinnedProgram(p).rules should contain(AspFact(a))
  }

  "A program with two different Facts a. b." should "be transformed into 2 rules a(T). b(T)." in {
    val p = LarsProgram.from(
      LarsFact(a),
      LarsFact(b)
    )

    DefaultLarsToPinnedProgram(p).rules should have size 2
  }
}
