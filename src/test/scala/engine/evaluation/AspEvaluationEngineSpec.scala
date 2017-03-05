package engine.evaluation

import engine.asp.{PinnedModel, PinnedModelToLarsModel, now}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class AspEvaluationEngineSpec extends FlatSpec with TimeTestFixtures {
  //val PinnedModelToLarsModel = engine.asp.PinnedModelToLarsModel(ClingoProgramWithLars(Set(), Seq(), 0))
  "An empty model" should "be empty afterwards" in {
    PinnedModelToLarsModel(t1, Set()) should have size 0
  }
  "now(T)" should "be removed from result-model" in {
    PinnedModelToLarsModel(t1, Set(now(T))) should have size 0
  }
  "now(t1)" should "be removed from result-model" in {
    PinnedModelToLarsModel(t1, Set(now(t1))) should have size 0
  }

  "An atom 'a(t1)'" should "be part of the result" in {
    PinnedModelToLarsModel(t1, Set(a(t1))) should contain only a
  }

  "An atom 'a(t0)'" should "not be part of the result at t1" in {
    PinnedModelToLarsModel(t1, Set(a(t0))) should have size 0
  }

  "A model containing a(1), now(0) and a(2)" should "be empty at t0" in {
    val model: PinnedModel = Set(a(t1), now(t0), a(t2))
    PinnedModelToLarsModel(t0, model) should have size 0
  }
}
