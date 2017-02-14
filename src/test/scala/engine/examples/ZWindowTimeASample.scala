package engine.examples

import core.Atom
import core.lars._
import engine._
import engine.config.BuildEngine
import fixtures._
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 26.04.16.
  */
class ZWindowTimeASample extends ConfigurableEvaluationSpec with TimeTestFixtures with TmsDirectPolicyEngine {
  val aspProgram =
    """
    z(X) :- w2ta(U,T), X = U + 1.
    w2ta(U,T) :- now(T), reach(U,T), a(U).
    reach(U,T) :- now(T), U=T-2..T.

    i(T) :- w1d_z(T).

    w1d_z(T) :- z(U), now(T), U >= T - 1, U <= T.

    #show a/1.
    #show z/1.
    #show i/1.
    """.stripMargin

  val aspExpressions = aspProgram.split('\n') toSet

  val w1d_a = Atom("w1d_a")

  /**
    *
    * ******** 1  2  3
    * ---------|--|--|--------------
    * *input** a
    * *output**** z
    * *output**** i  i
    */
  val program = LarsProgram.from(
    AtAtom(TimeVariableWithOffset(U) + 1, z) <= W(2, At(U), a),
    i <= W(1, Diamond, z)
  )

  // TODO: these test cases fail with grounding
  
  def preparedEngine = {
    info("Given 't1 -> a' ")
    evaluationEngine.append(t1)(a)

    evaluationEngine
  }

  //  pendingWithTms("Missing grounding of Variable U")
  it should "not lead to z at t0" in {
    evaluationEngine.evaluate(t0).get shouldNot contain(z)
  }

  it should "not lead to z at t1" in {
    preparedEngine.evaluate(t1).get.value shouldNot contain(z)
  }

  it should "lead to z and i at t2" in {
    preparedEngine.evaluate(t2).get.value should contain allOf(z, i)
  }

  it should "not lead to z but to i at t3" in {
    val result = preparedEngine.evaluate(t3).get.value

    result should contain(i)
    result shouldNot contain(z)
  }

  it should "not lead to z or i at t4" in {
    val result = preparedEngine.evaluate(t4).get.value

    result shouldNot contain allOf(i, z)
  }

  //
  //  val clingoBaseConfig = BuildEngine.withProgram(larsProgram).useAsp().withClingo().use()
  //  "Using Clingo-Pull" should behave like evaluation(clingoBaseConfig.usePull().start())
  //  "Using Clingo-Push" should behave like evaluation(clingoBaseConfig.usePush().start())
  //
  //  val tmsBaseConfig = BuildEngine.withProgram(larsProgram).useAsp().withTms().use()
  //  "Using ASP-TMS pull" should behave like evaluation(tmsBaseConfig.usePull().start())
  //  "Using ASP-TMS Push" should behave like evaluation(tmsBaseConfig.usePush().start())

}
