package util

import java.util.concurrent.TimeUnit

import core.Model

import scala.concurrent.duration.Duration

/**
  * Created by FM on 21.07.16.
  */
case class DurationSeries(durations: Seq[Duration]) extends NumberSeries[Duration] {
  override val max: Duration = durations.max
  override val min: Duration = durations.min
  override val avg: Duration = (durations.foldLeft(Duration.Zero.asInstanceOf[Duration])((s, d) => d + s)) / durations.length.toDouble
  override val median: Duration = durations.sorted.drop(durations.length / 2).head
  override val total: Duration = Duration.create(durations.map(_.toMillis).sum, TimeUnit.MILLISECONDS)

  def +(other: DurationSeries): DurationSeries = {
    assert(this.durations.size == other.durations.size)
    val newDurations: Seq[Duration] = this.durations.zip(other.durations).map{ pair => pair._1 + pair._2 }
    DurationSeries(newDurations)
  }

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

object DurationSeries {
  def fromDurations(durationSeq: Seq[Duration]): DurationSeries = {
    if (durationSeq.isEmpty) {
      DurationSeries(Seq(Duration.Zero))
    } else {
      DurationSeries(durationSeq)
    }
  }

  def fromMillis(seq: Seq[Long]): DurationSeries = fromDurations(seq.map(toDuration))
  def fromMillisDoubles(seq: Seq[Double]): DurationSeries = fromDurations(seq.map(toDuration))

  private def toDuration(millis: Long) = Duration.create(millis, TimeUnit.MILLISECONDS)
  private def toDuration(double: Double) = Duration.create(double, TimeUnit.MILLISECONDS)
}

trait ConfigurationResult {
  val instanceCaption: String
}

case class TimingsConfigurationResult(instanceCaption: String, appendResult: DurationSeries, evaluateResult: DurationSeries) extends ConfigurationResult

case class SuccessConfigurationResult(instanceCaption: String, successFailures: Seq[(Int, Boolean)]) extends ConfigurationResult

case class ModelsResult(instanceCaption: String, models: Seq[(Int, Option[Model])]) extends ConfigurationResult

case class UnequalResult(instanceCaption: String, unequalModels: Seq[(Int, Option[Model], Option[Model])]) extends ConfigurationResult

case class AlgorithmResult[TResult <: ConfigurationResult](caption: String, runs: Seq[TResult])

