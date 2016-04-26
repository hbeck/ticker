package engine.examples

import asp.AspExpression
import core.{Atom, Program, not}
import engine.{At, Time}
import engine.implementations.{AspPullEvaluation, StreamingAspTransformation}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 22.04.16.
  */
class XWindowBoxASample extends FlatSpec {
  val aspProgram =
    """x(T) :- w1b_a(T).

       w1b_a(T) :- now(T), not spoil_w1b_a(T).
       spoil_w1b_a(T) :- reach_w1b_a(U,T), not a(U).
       reach_w1b_a(U,T) :- now(T), U=T-1..T.

      #show a/1.
      #show x/1.
    """

  val aspExpressions = aspProgram.split('\n') map (x => AspExpression(x)) toSet


  val x = Atom("x")
  val w1b_a = Atom("w1b_a")
  val spoil_w1b_a = Atom("spoil_w1b_a")
  val a = Atom("a")
  val u = Atom("u")

  val now = StreamingAspTransformation.now

  val program = Program(
    x("T") :- w1b_a("T"),
    w1b_a("T") :- now("T") and not(spoil_w1b_a("T")),
    spoil_w1b_a("T") :- now("T") and u("U") and not(a("U"))
  )


  val t0 = Time(0)
  val t1 = Time(1)
  val t2 = Time(2)

  def evaluation = {
    val e = AspPullEvaluation(StreamingAspTransformation(aspExpressions))

    e.append(t1)(a)
    e.append(t2)(a)

    e
  }

  "Given '{t1 -> a}, {t2 -> a}' " should "not lead to x at t0" in {
    evaluation.evaluate(t0).get shouldNot contain(x("0"))
  }

  it should "not lead to x at t1" in {
    evaluation.evaluate(t1).get.value shouldNot contain(x("1"))
  }

  it should "lead to x at t2" in {
    evaluation.evaluate(t2).get.value should contain(x("2"))
  }
}
