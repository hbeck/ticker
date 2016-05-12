package lars.transform

import core.asp.AspFact
import core.lars.{Fact, Program}
import engine.TransformLars
import org.scalatest.Matchers._

/**
  * Created by FM on 12.05.16.
  */
class ProgramSpec extends TransformLarsSpec {

  "A program with one Fact a." should "be tranformed into 2 rules" in {
    val p = Program(Fact(a))

    TransformLars(p, t1) should have size 2
  }
  it should "contain now(t1)" in {
    val p = Program(Fact(a))

    TransformLars(p, t1) should contain(AspFact(now(t1.toString)))
  }
  it should "contain a(T)." in {
    val p = Program(Fact(a))

    TransformLars(p, t1) should contain(AspFact(a(T)))
  }

  "A program with two different Facts a. b." should "be transformed into 3 rules a. b. now(t1)." in {
    val p = Program(
      Fact(a),
      Fact(b)
    )

    TransformLars(p, t1) should have size 3
  }
}
