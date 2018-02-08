package reasoner

import core.Atom
import core.lars._
import fixtures.{ClingoPullReasoner, TimeTestFixtures}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 11.12.16.
  */
class AtExamples extends FlatSpec with TimeTestFixtures with ClingoPullReasoner {

  def TimeW(windowSize: TimeWindowSize) = SlidingTimeWindow(windowSize)

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

  it should "hold w 3m @37m busG at all time points t ∈ [37.2m, 40.2m]." in {

    // TODO: discuss if we should use lookahead for concrete time points???
    val program = LarsProgram.from(
      temp <= WindowAtom(TimeW(3), At(t37), busG)
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

  "For { 37m → {busG} }  @_{T +3m} expBusM " should "hold for w6 3m @T busG at T_40" in {
    val expBusM = Atom("expBusM")
    val U = TimeVariableWithOffset("U")

    val program = LarsProgram.from(
      AtAtom(U + 3, expBusM) <= WindowAtom(TimeW(3), At(U), busG)
    )

    val engine = defaultEngine(program)

    engine.append(t37)(busG)

    engine.evaluate(t37).get.get should not contain (expBusM)
    engine.evaluate(38).get.get should not contain (expBusM)
    engine.evaluate(39).get.get should not contain (expBusM)

    engine.evaluate(40).get.get should contain(expBusM)
  }

  "For { 37 → {a} }  @_{T - 1} b :- w^3 @T a. c :- w^2 d b." should "hold c for T_37, T_38" in {
    val U = TimeVariableWithOffset("U")

    val program = LarsProgram.from(
      AtAtom(U - 1, b) <= WindowAtom(TimeW(3), At(U), a),
      c <= WindowAtom(TimeW(2), Diamond, b)
    )

    val engine = defaultEngine(program)

    engine.append(t37)(a)

    // t_36 is not possible to evaluate (before timestamp t_37)
    engine.evaluate(37).get.get should contain(c)
    engine.evaluate(38).get.get should contain(c)

    engine.evaluate(39).get.get should not contain (c)
  }
}
