package engine.examples

import asp.{AspConversion, AspExpression}
import core.{Atom, Program, not}
import engine.At
import engine.implementations.StreamingAspTransformation
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 23.04.16.
  */
class YWindowDiamondASample extends FlatSpec {
  val aspProgram =
    """y(T) :- w1d_a(T).

      w1d_a(T) :- a(U), now(T), U >= T - 1, U <= T.

      #show a/1.
      #show y/1.
    """

  val aspExpressions = aspProgram.split('\n') map (x => AspExpression(x)) toSet

  val y = Atom("y")
  val w1d_a = Atom("w1d_a")
  val a = Atom("a")
  val u = Atom("u")

  val now = StreamingAspTransformation.now

  val program = Program(
    y("T") :- w1d_a("T"),
    w1d_a("T") :- a("U") and now("T") and u("U")
  )
  val t0 = At.second(0)
  val t1 = At.second(1)
  val t2 = At.second(2)

  val conversion = StreamingAspTransformation(aspExpressions)

  "Given 't1 -> a' " should "not lead to y at t0" in {
    conversion.prepare(t0, Set()).get shouldNot contain(y)
  }

  it should "lead to y at t1" in {
    conversion.prepare(t1, Set(a)).get.value should contain(y("1000"))
  }

  it should "lead to y at t2" in {
    conversion.prepare(t2, Set()).get.value should contain(y("2000"))
  }
}
