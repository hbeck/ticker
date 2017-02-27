package evaluation

import java.util.concurrent.TimeUnit

import core.Model

import scala.concurrent.duration.Duration

/**
  * Created by FM on 21.07.16.
  */
case class StatisticResult(executionTimes: Seq[Duration]) {
  val max = executionTimes.max
  val min = executionTimes.min
  val avg = executionTimes.foldLeft(Duration.Zero.asInstanceOf[Duration])((s, d) => d + s) / executionTimes.length.toDouble
  val median = executionTimes.sorted.drop(executionTimes.length / 2).head

  override def toString = {
    val unit = TimeUnit.MILLISECONDS
    val b = StringBuilder.newBuilder
      .append(f"Results [${unit.name()}]\n")
      .append(f"Max: ${max.toUnit(unit)}\n")
      .append(f"Min: ${min.toUnit(unit)}\n")
      .append(f"Average: ${avg.toUnit(unit)}\n")
      .append(f"Median: ${median.toUnit(unit)}\n")

    b.toString()
  }

  def asResult(unit: TimeUnit = TimeUnit.MILLISECONDS) = {

    val results = Seq(
      min,
      max,
      avg,
      median
    )

    results map (_.toUnit(unit))
  }
}

object StatisticResult {
  def fromExecutionTimes(executionTimes: Seq[Duration]): StatisticResult = {
    if (executionTimes.isEmpty) {
      StatisticResult(Seq(Duration.Zero))
    } else
      StatisticResult(executionTimes)
  }
}

trait ConfigurationResult {
  val instanceCaption: String
}

case class TimingsConfigurationResult(instanceCaption: String, appendResult: StatisticResult, evaluateResult: StatisticResult) extends ConfigurationResult

case class SuccessConfigurationResult(instanceCaption: String, successFailures: Seq[(Int, Boolean)]) extends ConfigurationResult

case class ModelsResult(instanceCaption: String, models: Seq[(Int, Option[Model])]) extends ConfigurationResult

case class UnequalResult(instanceCaption: String, unequalModels: Seq[(Int, Option[Model], Option[Model])]) extends ConfigurationResult

case class AlgorithmResult[TResult <: ConfigurationResult](caption: String, runs: Seq[TResult])