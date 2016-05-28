package engine

import core.Atom
import core.asp.AspProgram
import core.lars.Program
import engine.asp.EvaluationStrategy
import engine.config.{AspEvaluationEngineConfiguration, BuildEngine}
import fixtures.{EvaluateProgramWithAllImplementations, TimeTestFixtures}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 21.04.16.
  */
class EngineStreamSpec extends FlatSpec with TimeTestFixtures with EvaluateProgramWithAllImplementations {

  val program = Program.from(
    a <= b,
    b <= c not d
  )

  def evaluate(evaluationEngine: => EvaluationEngine) {

    it should "lead to different evaluation results" in {
      info("Adding atoms one after another at the same timepoint")

      val engine = evaluationEngine

      val atT1 = engine.append(t1) _

      atT1(Seq(Atom("c")))

      assume(Set(a, b, c) subsetOf engine.evaluate(t1).get.value)

      atT1(Seq(Atom("d")))

      engine.evaluate(t1).get.value should contain allOf(c, d)
    }

    it should "not lead to a result at t3" in {
      info("Adding one atom at t2")
      val engine = evaluationEngine

      engine.append(t2)(Atom("c"))

      assume(Set(a, b, c).subsetOf(engine.evaluate(t2).get.value))

      assert(engine.evaluate(t3).get.value.isEmpty)
    }

    it should "not lead to a result when evaluating at t1" in {
      info("Adding one atom at t2")
      val engine = evaluationEngine

      engine.append(t2)(Atom("c"))

      assume(Set(a, b, c) subsetOf engine.evaluate(t2).get.value)

      assert(engine.evaluate(t1).get.value.isEmpty)
    }
  }

  "Engine Stream Specs" must behave like runInAllImplementations(program)(evaluate)
}
