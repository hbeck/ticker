package evaluation.reachBlocked

import java.util.concurrent.TimeUnit

import core.Atom
import core.lars.TimePoint
import evaluation.RunWithAllImplementations
import fixtures.{ClingoPushEngine, ConfigurableEvaluationSpec, JtmsGreedyLazyRemovePolicyEngine, TimeTestFixtures}
import util.{StatisticResult, TimedEvaluationEngine}

import scala.language.implicitConversions
import scala.concurrent.duration.{Deadline, Duration}


/**
  * Created by FM on 11.07.16.
  */
class StreamingObstacles extends ConfigurableEvaluationSpec with TimeTestFixtures with ClingoPushEngine with ParallelLanes {
  val program = generateProgramWithGrounding(3, 3)

  val obstacles = generatedNodes.map(obstacle(_)).toSet.subsets().toList

  def executeSample(engine: TimedEvaluationEngine) = {
    obstacles zip (Stream from 1) foreach (t => engine.append(t._2)(t._1.toSeq: _*))
  }

  "All different combinations of obstacles" should "be appended at a given timepoint"  ignore{

    val timedEngine = TimedEvaluationEngine(evaluationEngine)

    executeSample(timedEngine)

    val d = StatisticResult.fromExecutionTimes(timedEngine.appendExecutionTimes)
    info(d.toString)
  }


}

//class AllStreamingObstacles extends RunWithAllImplementations(new StreamingObstacles)