package engine

import core.{Atom, Variable}
import core.lars._
import org.scalatest.FlatSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._
import fixtures.{ConfigurableEvaluationSpec, TimeTestFixtures, TmsDirectPolicyEngine}

/**
  * Created by FM on 11.12.16.
  */
class AtExamples extends FlatSpec with TimeTestFixtures with TmsDirectPolicyEngine {
  val busG = Atom("busG")
  val tramB = Atom("tramB")
  val t37 = TimePoint(37)
  val t39 = TimePoint(39)

  val temp = Atom("temp")

  "The ijcai sample for { 37m → {busG} }" should "hold @37m busG at any timepoint" in {
    val program = LarsProgram.from(
      temp <= AtAtom(t37, busG)
    )

    val engine = defaultEngine(program)

    engine.append(t37)(busG)

    engine.evaluate(t37).get.get should contain(temp)
    engine.evaluate(38).get.get should contain(temp)
    engine.evaluate(39).get.get should contain(temp)
    engine.evaluate(40).get.get should contain(temp)
    engine.evaluate(41).get.get should contain(temp)
  }
  t
  it should "hold w 3m @37m busG at all time points t ∈ [37.2m, 40.2m]." in {
    val program = LarsProgram.from(
      temp <= WindowAtom(STW(3), At(t37), busG)
    )

    val engine = defaultEngine(program)

    engine.append(t37)(busG)

    engine.evaluate(t37).get.get should contain(temp)
    engine.evaluate(38).get.get should contain(temp)
    engine.evaluate(39).get.get should contain(temp)
    engine.evaluate(40).get.get should contain(temp)

    engine.evaluate(41).get.get should not contain (temp)
    engine.evaluate(42).get.get should not contain (temp)
  }

  "For { 37m → {busG} }  @_{T +3m} expBusM " should "hold for w 3m @37m busG" in {
    val expBusM = Atom("expBusM")
    val U = TimeVariableWithOffset("U")

    val program = LarsProgram.from(
      AtAtom(U + 3, expBusM) <= WindowAtom(STW(4), At(U), busG)
    )

    val engine = defaultEngine(program)

    engine.append(t37)(busG)

    engine.evaluate(t37).get.get should not contain (expBusM)
    engine.evaluate(38).get.get should not contain (expBusM)
    engine.evaluate(39).get.get should not contain (expBusM)

    engine.evaluate(40).get.get should contain(expBusM)

  }
}
