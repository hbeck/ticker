package evaluation.reachBlocked

import java.util.concurrent.TimeUnit

import core.Atom
import core.lars.TimePoint
import evaluation.{RunWithAllImplementations, StatisticResult}
import fixtures.{ClingoPushEngine, ConfigurableEvaluationSpec, TimeTestFixtures, TmsLazyRemovePolicyEngine}

import scala.language.implicitConversions
import scala.concurrent.duration.{Deadline, Duration}


/**
  * Created by FM on 11.07.16.
  */
class StreamingObstacles extends ConfigurableEvaluationSpec with TimeTestFixtures with ClingoPushEngine with ParallelLanes {
  val program = generateProgram(2, 3)

  val obstacles = generatedNodes.map(obstacle(_)).toSet.subsets().toList

  var executionTimes: List[Duration] = List()

  "All different combinations of obstacles" should "be appended at a given timepoint" in {
    obstacles zip (Stream from 1) foreach (t => timedAppend(t._2, t._1.toSeq))

    val d = StatisticResult.fromExecutionTimes(executionTimes)
    info(d.toString)
  }


  def append(time: TimePoint, atoms: Seq[Atom]) = {
    evaluationEngine.append(time)(atoms: _*)
  }

  def timedAppend(time: TimePoint, atoms: Seq[Atom]) = {
    val start = Deadline.now

    append(time, atoms)

    val end = Deadline.now

    val elapsed = ((end - start))
    executionTimes = executionTimes :+ elapsed
  }
}

class AllStreamingObstacles extends RunWithAllImplementations(new StreamingObstacles)