package engine

import core.Atom
import core.asp.AspProgram
import core.lars.LarsProgram
import engine.asp.oneshot.EvaluationStrategy
import engine.config.{AspEngineEvaluationConfiguration, BuildEngine, EngineEvaluationConfiguration}
import fixtures._
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.OutcomeOf._
import org.scalatest.events.Event
import org.scalatest.tools.StandardOutReporter


/**
  * Created by FM on 21.04.16.
  */
class EngineStreamSpec extends ConfigurableEvaluationSpec with TimeTestFixtures with ClingoPullEngine {

  val program = LarsProgram.from(
    a <= b,
    b <= c not d
  )


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

  it should "not lead to a result when evaluating at t1" in pendingWithTms("Querying after an already evaluated time point works only with true one-shot reasoning") {
    info("Adding one atom at t2")
    val engine = evaluationEngine

    engine.append(t2)(Atom("c"))

    assume(Set(a, b, c) subsetOf engine.evaluate(t2).get.value)

    assert(engine.evaluate(t1).get.value.isEmpty)
  }

}
