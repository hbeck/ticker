package util

case class DoubleSeries(doubles: Seq[Double]) extends NumberSeries[Double] {

  lazy val min: Double = doubles.min
  lazy val max: Double = doubles.max
  lazy val avg: Double = if (doubles.isEmpty) {0.0} else { (1.0*doubles.sum)/(1.0*doubles.length) }
  lazy val median: Double = if (doubles.isEmpty) {0.0} else { doubles.sorted.drop(doubles.length / 2).head }
  lazy val total: Double = doubles.sum

}
