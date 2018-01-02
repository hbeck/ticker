package reasoner.examples

import core.Atom
import core.lars._
import fixtures._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 26.04.16.
  */
class ZWindowTimeASample extends ConfigurableEngineSpec with TimeTestFixtures with TmsDirectPolicyEngine {

  /**
    *
    * ******** 1  2  3
    * ---------|--|--|--------------
    * *input** a
    * *output**** z
    * *output**** i  i
    */
  //TODO obs hb: shouldn't make a difference in pinning whether T or U is used (but is!)
  val program = LarsProgram.from(
    AtAtom(TimeVariableWithOffset(U) + 1, z) <= W(2, At(U), a), //@_{U+1}z :- w2 @_U a
    //AtAtom(TimeVariableWithOffset(T,1), z) <= W(2, At(T), a), //TODO hb: must also work! (T vs U)
    i <= W(1, Diamond, z) //i :- w1 D z
  )

  // TODO: these test cases fail with grounding

  def preparedEngine = {
    info("Given 't1 -> a' ")
    reasoner.append(t1)(a)

    reasoner
  }

  //  pendingWithTms("Missing grounding of Variable U")
  it should "not lead to z at t0" in {
    reasoner.evaluate(t0).get shouldNot contain(z)
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
  //  "Using Clingo-Pull" should behave like experimental.evaluation(clingoBaseConfig.usePull().start())
  //  "Using Clingo-Push" should behave like experimental.evaluation(clingoBaseConfig.usePush().start())
  //
  //  val tmsBaseConfig = BuildEngine.withProgram(larsProgram).useAsp().withTms().use()
  //  "Using ASP-TMS pull" should behave like experimental.evaluation(tmsBaseConfig.usePull().start())
  //  "Using ASP-TMS Push" should behave like experimental.evaluation(tmsBaseConfig.usePush().start())

}
