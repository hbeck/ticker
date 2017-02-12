package engine.examples

import core.Atom
import core.asp.AspProgram
import core.lars._
import engine.EvaluationEngine
import engine.asp.now
import engine.config.BuildEngine
import evaluation.{ RunWithAllImplementations}
import fixtures._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 23.04.16.
  */
class YWindowDiamondASample extends ConfigurableEvaluationSpec with TimeTestFixtures with ClingoPullEngine {
  val aspStringProgram =
    """y(T) :- w1d_a(T).

      w1d_a(T) :- a(U), now(T), U >= T - 1000, U <= T.

      #show a/1.
      #show y/1.
    """

  val aspExpressions = aspStringProgram.split('\n') toSet

  val w1d_a = Atom("w1d_a")

  val u = Atom("u")

  val aspProgram = AspProgram(
    y("T") :- w1d_a("T"),
    w1d_a("T") :- a("U") and now("T") and u("U")
  )

  val program = LarsProgram.from(
    y <= W(1, Diamond, a)
  )

  def engineWithStream = {
    info("Given 't1 -> a' ")

    evaluationEngine.append(t1)(a)

    evaluationEngine
  }

  "An empty program" should "not lead to y at t0" in {
    evaluationEngine.evaluate(t0).get shouldNot contain(y)
  }

  it should "lead to y at t1" in {
    engineWithStream.evaluate(t1).get.value should contain(y)
  }

  it should "lead to y at t2" in {
    engineWithStream.evaluate(t2).get.value should contain(y)
  }

  it should "not contain y at t3" in {
    engineWithStream.evaluate(t3).get.value shouldNot contain(y)
  }

}
