package engine.examples

import core.Atom
import core.asp.AspProgram
import core.lars._
import engine.EvaluationEngine
import engine.asp.now
import engine.config.BuildEngine
import fixtures.{ClingoPullEngine, ConfigurableEvaluationSpec, TimeTestFixtures, TmsDirectPolicyEngine}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 22.04.16.
  */
class XWindowBoxASample extends ConfigurableEvaluationSpec with TimeTestFixtures with TmsDirectPolicyEngine {
  val aspStringProgram =
    """x(T) :- w1b_a(T).

       w1b_a(T) :- now(T), not spoil_w1b_a(T).
       spoil_w1b_a(T) :- reach_w1b_a(U,T), not a(U).
       reach_w1b_a(U,T) :- now(T), U=T-1..T.

      #show a/1.
      #show x/1.
    """

  val aspExpressions = aspStringProgram.split('\n') toSet

  val w1b_a = Atom("w1b_a")
  val spoil_w1b_a = Atom("spoil_w1b_a")

  val u = Atom("u")

  val aspProgram = AspProgram(
    x("T") :- w1b_a("T"),
    w1b_a("T") :- now("T") not (spoil_w1b_a("T")),
    spoil_w1b_a("T") :- now("T") and u("U") not (a("U"))
  )

  val program = LarsProgram.from(
    x <= WindowAtom(SlidingTimeWindow(1), Box, a)
  )

  info ( "Engine " + this.evaluationType)

  def engineWithStream = {
    info("Given '{t1 -> a}, {t2 -> a}' ")

    evaluationEngine.append(t1)(a)
    evaluationEngine.append(t2)(a)

    evaluationEngine
  }


  "An empty program" should "not lead to x at t0" in {
    evaluationEngine.evaluate(t0).get shouldNot contain(x)
  }

  it should "not lead to x at t1" in {
    engineWithStream.evaluate(t1).get.value shouldNot contain(x)
  }

  it should "lead to x at t2" in {
    engineWithStream.evaluate(t2).get.value should contain(x)
  }
  it should "not contain x(2) at t3" in {
    val model = engineWithStream.evaluate(t3).get
    model.value shouldNot contain(x)
  }
}
