package engine.examples

import core.asp.AspProgram
import core.lars._
import core.{Atom, asp}
import engine.EvaluationEngine
import engine.asp.evaluation.{AspEvaluationEngine, StreamingClingoInterpreter}
import engine.asp.{AspPullEvaluationEngine, PlainLarsToAsp, now}
import engine.config.BuildEngine
import engine._
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 22.04.16.
  */
class XWindowBoxASample extends FlatSpec with TimeTestFixtures {
  val aspProgram =
    """x(T) :- w1b_a(T).

       w1b_a(T) :- now(T), not spoil_w1b_a(T).
       spoil_w1b_a(T) :- reach_w1b_a(U,T), not a(U).
       reach_w1b_a(U,T) :- now(T), U=T-1..T.

      #show a/1.
      #show x/1.
    """

  val aspExpressions = aspProgram.split('\n') toSet

  val w1b_a = Atom("w1b_a")
  val spoil_w1b_a = Atom("spoil_w1b_a")

  val u = Atom("u")

  val program = AspProgram(
    x("T") :- w1b_a("T"),
    w1b_a("T") :- now("T") not (spoil_w1b_a("T")),
    spoil_w1b_a("T") :- now("T") and u("U") not (a("U"))
  )

  val larsProgram = Program.from(
    x <= WindowAtom(SlidingTimeWindow(1), Box, a)
  )

  def evaluation(evaluationEngine: EvaluationEngine) = {

    evaluationEngine.append(t1)(a)
    evaluationEngine.append(t2)(a)

    info("Given '{t1 -> a}, {t2 -> a}' ")

    it should "not lead to x at t0" in {
      evaluationEngine.evaluate(t0).get shouldNot contain(x("0"))
    }

    it should "not lead to x at t1" in {
      evaluationEngine.evaluate(t1).get.value shouldNot contain(x("1"))
    }

    it should "lead to x at t2" in {
      evaluationEngine.evaluate(t2).get.value should contain(x("2"))
    }
    it should "not contain x(2) at t3" in {
      val model = evaluationEngine.evaluate(TimePoint(3)).get
      model.value shouldNot contain(x("2"))
    }
  }

  val clingoBaseConfig = BuildEngine.withProgram(larsProgram).useAsp().withClingo().use()
  "Using Clingo-Pull" should behave like evaluation(clingoBaseConfig.usePull().start())
  "Using Clingo-Push" should behave like evaluation(clingoBaseConfig.usePush().start())

  val tmsBaseConfig = BuildEngine.withProgram(larsProgram).useAsp().withTms().use()
  "Using ASP-TMS pull" should behave like evaluation(tmsBaseConfig.usePull().start())
  "Using ASP-TMS Push" should behave like evaluation(tmsBaseConfig.usePush().start())
}
