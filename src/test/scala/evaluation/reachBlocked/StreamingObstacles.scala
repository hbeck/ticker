package evaluation.reachBlocked

import java.util.concurrent.TimeUnit

import core.Atom
import core.lars.TimePoint
import fixtures.{ClingoPushEngine, ConfigurableEvaluationSpec, TimeTestFixtures, TmsLazyRemovePolicyEngine}

import scala.language.implicitConversions
import scala.concurrent.duration.{Deadline, Duration}

case class StatisticResult(max: Duration, min: Duration, avg: Duration, median: Duration) {
  override def toString = {
    val unit = TimeUnit.MILLISECONDS
    val b = StringBuilder.newBuilder
      .append(f"Max: ${max.toUnit(unit)}\n")
      .append(f"Min: ${min.toUnit(unit)}\n")
      .append(f"Average: ${avg.toUnit(unit)}\n")
      .append(f"Median: ${median.toUnit(unit)}\n")

    b.toString()
  }
}

/**
  * Created by FM on 11.07.16.
  */
class StreamingObstacles extends ConfigurableEvaluationSpec with TimeTestFixtures with ClingoPushEngine with ParallelLanes {
  val program = generateProgram(3, 3)

  val obstacles = generatedNodes.map(obstacle(_)).toSet.subsets().toList

  var executionTimes: List[Duration] = List()

  it should "work" in {
    obstacles zip (Stream from 1) foreach (t => timedAppend(t._2, t._1.toSeq))

    val d = calculate()
    info(d.toString)
  }

  def calculate() = {
    if (executionTimes.isEmpty) {
      StatisticResult(Duration.Zero, Duration.Zero, Duration.Zero, Duration.Zero)
    } else
      StatisticResult(
        max = executionTimes.max,
        min = executionTimes.min,
        avg = (executionTimes.foldLeft(Duration.Zero.asInstanceOf[Duration])((s, d) => d.+(s)) / executionTimes.length.toDouble),
        median = executionTimes.sorted.drop(executionTimes.length / 2).head
      )
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
