package evaluation.reachBlocked

import evaluation.RunWithAllImplementations
import fixtures.{ConfigurableEvaluationSpec, JtmsGreedyLazyRemovePolicyEngine, TimeTestFixtures}
import org.scalatest.Matchers._

/**
  * Created by FM on 20.06.16.
  */
//@NoTmsDirectPolicy
class ParallelLanesSample extends ConfigurableEvaluationSpec with TimeTestFixtures with JtmsGreedyLazyRemovePolicyEngine with ParallelLanes {

  val program = generateProgramWithGrounding(3, 3)

  "Unblocked paths" should "lead to reach(a,b) at t0" in {
    evaluationEngine.evaluate(t0).get.get should contain(reach_a_b)
  }

  "A single obstacle at one lane at t0" should "still lead to reach(a,b) at t0...t5" in {
    evaluationEngine.append(t0)(obstacle(nodesAt(0).head))

    evaluationEngine.evaluate(t0).get.get should contain(reach_a_b)
  }

  "Obstacles at all paths at t0" should "not lead to reach(a,b) at t0" in {
    val atT0 = evaluationEngine.append(t0) _

    (0 to lanes - 1) foreach (l => atT0(Seq(obstacle(nodesAt(l).head))))

    evaluationEngine.evaluate(t0).get.get should not contain (reach_a_b)
  }

  "Obstacles at all paths at t0" should "lead to reach(a,b) at t6" in {
    val atT0 = evaluationEngine.append(t0) _

    (0 to lanes - 1) foreach (l => atT0(Seq(obstacle(nodesAt(l).head))))

    evaluationEngine.evaluate(6).get.get should contain(reach_a_b)
  }
}


class AllParallelLanes extends RunWithAllImplementations(new ParallelLanesSample)