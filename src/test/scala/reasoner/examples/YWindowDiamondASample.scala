package reasoner.examples

import core.Atom
import core.asp.AspProgram
import core.lars._
import fixtures._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import reasoner.now

/**
  * Created by FM on 23.04.16.
  */
class YWindowDiamondASample extends ConfigurableReasonerSpec with TimeTestFixtures with JtmsIncrementalReasoner {
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

    reasoner.append(t1)(a)

    reasoner
  }

  "An empty program" should "not lead to y at t0" in {
    reasoner.evaluate(t0).get shouldNot contain(y)
  }

  it should "lead to y at t1" in {
    engineWithStream.evaluate(t1).get.value should contain(y)
  }

  it should "lead to y at t2" in {
    engineWithStream.evaluate(t2).get.value should contain(y)
  }

  it should "not contain y at t3" in {
    val initializedEngine = engineWithStream
    // TODO this fails with Jtms-Incremental because of incorrect remove of grounded-at-atoms
    initializedEngine.evaluate(t2)
    initializedEngine.evaluate(t3).get.value shouldNot contain(y)
  }

}
