package engine.examples

import core.{Atom, StringValue}
import core.lars._
import fixtures.{ConfigurableEvaluationSpec, TimeTestFixtures, TmsLazyRemovePolicyEngine}
import org.scalatest.Matchers._
import org.scalatest.Inspectors._
import org.scalatest.OptionValues._

/**
  * Created by FM on 23.08.16.
  */
class RunningFluentSample extends ConfigurableEvaluationSpec with TimeTestFixtures with TmsLazyRemovePolicyEngine {
  val on = StringValue("on")
  val off = StringValue("off")

  // TODO: this needs to be variable if grounding works
  val M = StringValue("M")

  val running = Atom("running")
  val op = Atom("op")

  val program = LarsProgram.from(
    running <= Fluent(op(M, on))
  )

  "No value present" should "not lead to running" in {
    evaluationEngine.evaluate(t0).get.value shouldBe empty
  }

  "{0->op(M,on)} present" should "lead to running" in {
    evaluationEngine.append(t0)(op(M, on))
    evaluationEngine.evaluate(t0).get.value should contain(running)
  }

  "{0->op(M,on), 1->op(M,off)}" should "not lead to running at t1" in {
    evaluationEngine.append(t0)(op(M, on))
    evaluationEngine.append(t1)(op(M, off))
    evaluationEngine.evaluate(t1).get.value should not contain (running)
  }

  "{0->op(M,on), 1->op(M,off), 2-> op(M, on)}" should "lead to running at t2-t3" in {
    evaluationEngine.append(t0)(op(M, on))
    evaluationEngine.append(t1)(op(M, off))
    evaluationEngine.append(t2)(op(M, on))
    evaluationEngine.evaluate(t2).get.value should contain (running)
    evaluationEngine.evaluate(t3).get.value should contain (running)
  }


}
