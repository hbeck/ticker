package experimental.evaluation

import core.lars.{Diamond, LarsProgram, W}
import engine.asp.tms.policies.LazyRemovePolicy
import engine.config.BuildReasoner
import fixtures.{ConfigurableEngineSpec, EngineBuilder, TimeTestFixtures}
import jtms.algorithms.JtmsGreedy
import jtms.networks.OptimizedNetwork
import org.scalatest.Inspectors._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

import scala.util.Random

/**
  * Created by FM on 09.06.16.
  */
class TmsPerformanceSample extends ConfigurableEngineSpec with TimeTestFixtures with EngineBuilder {
  val program = LarsProgram.from(
    a <= b,
    b <= c,
    c <= d,
    d <= e,
    e <= f,
    f <= g,
    g <= h,
    h <= i,
    i <= j,
    j <= W(100, Diamond, k)
  )
  val defaultEngine = (p: LarsProgram) => BuildReasoner.
    withProgram(p).
    configure().
    withJtms().
    withPolicy(LazyRemovePolicy(new JtmsGreedy(new OptimizedNetwork(), new Random(1)), 10)).
    seal()

  "An empty Program" should "lead to an empty model at t0" in {
    engine.evaluate(t0).get.value shouldBe empty
  }

  "{1 -> k}" should "lead to model a for 1...100" in {
    engine.append(t1)(k)

    forAll(1 to 100) { t => engine.evaluate(t).get.value should contain(a) }
  }
}

class AllPerformance extends RunWithAllImplementations(new TmsPerformanceSample)
