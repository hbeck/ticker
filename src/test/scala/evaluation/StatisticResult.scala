package evaluation

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

/**
  * Created by FM on 21.07.16.
  */
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

object StatisticResult {
  def fromExecutionTimes(executionTimes: List[Duration]): StatisticResult = {
    if (executionTimes.isEmpty) {
      StatisticResult(Duration.Zero, Duration.Zero, Duration.Zero, Duration.Zero)
    } else
      StatisticResult(
        max = executionTimes.max,
        min = executionTimes.min,
        avg = executionTimes.foldLeft(Duration.Zero.asInstanceOf[Duration])((s, d) => d + s) / executionTimes.length.toDouble,
        median = executionTimes.sorted.drop(executionTimes.length / 2).head
      )
  }
}
