package engine.examples

import core.Atom
import core.asp.AspProgram
import core.lars._
import engine.EvaluationEngine
import engine.asp.now
import engine.config.BuildEngine
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 23.04.16.
  */
class YWindowDiamondASample extends FlatSpec with TimeTestFixtures {
  val aspProgram =
    """y(T) :- w1d_a(T).

      w1d_a(T) :- a(U), now(T), U >= T - 1000, U <= T.

      #show a/1.
      #show y/1.
    """

  val aspExpressions = aspProgram.split('\n') toSet

  val w1d_a = Atom("w1d_a")

  val u = Atom("u")

  val program = AspProgram(
    y("T") :- w1d_a("T"),
    w1d_a("T") :- a("U") and now("T") and u("U")
  )

  val larsProgram = Program.from(
    y <= W(1, Diamond, a)
  )

  def evaluation(evaluation: EvaluationEngine) = {
    evaluation.append(t1)(a)

    info("Given 't1 -> a' ")

    it should "not lead to y at t0" in {
      evaluation.evaluate(t0).get shouldNot contain(y)
    }

    it should "lead to y at t1" in {
      evaluation.evaluate(t1).get.value should contain(y)
    }

    it should "lead to y at t2" in {
      evaluation.evaluate(t2).get.value should contain(y)
    }

    it should "not contain y at t3" in {
      evaluation.evaluate(t3).get.value shouldNot contain(y)
    }

  }

  val clingoBaseConfig = BuildEngine.withProgram(larsProgram).useAsp().withClingo().use()
  "Using Clingo-Pull" should behave like evaluation(clingoBaseConfig.usePull().start())
  "Using Clingo-Push" should behave like evaluation(clingoBaseConfig.usePush().start())

  val tmsBaseConfig = BuildEngine.withProgram(larsProgram).useAsp().withTms().use()
  "Using ASP-TMS pull" should behave like evaluation(tmsBaseConfig.usePull().start())
  "Using ASP-TMS Push" should behave like evaluation(tmsBaseConfig.usePush().start())
}
