package util

trait NumberSeries[T] {

  def min: T
  def max: T
  def avg: T
  def median: T
  def total: T

}
