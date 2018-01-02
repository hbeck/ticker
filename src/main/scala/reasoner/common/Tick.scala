package reasoner.common

import reasoner.asp.Void

/**
  * Created by hb on 05.03.17.
  *
  * Special logic to introduce irrelevance/infinity with -1
  *
  */
case class Tick(time: Long, count: Long) {
  def + (other: Tick): Tick = {
    //ignoring -1 as number
    def add(n1: Long, n2: Long) = {
      if (n1 == Void || n2 == Void) Void
      else n1 + n2
    }
    Tick(add(this.time,other.time),add(this.count,other.count))
  }

  def incrementTime(): Tick = Tick(time+1,count)
  def incrementCount(): Tick = Tick(time,count+1)
}

object Tick {

  def min(p1: Tick, p2: Tick) = {
    val time = minWithVoidAsInfinity(p1.time,p2.time)
    val count = minWithVoidAsInfinity(p1.count,p2.count)
    Tick(time,count)
  }

  def max(p1: Tick, p2: Tick) = {
    val time = maxWithVoidAsInfinity(p1.time,p2.time)
    val count = maxWithVoidAsInfinity(p1.count,p2.count)
    Tick(time,count)
  }

  def minWithVoidAsInfinity(i: Long, j: Long): Long = {
    if (i == Void) return j
    if (j == Void) return i
    return math.min(i,j)
  }

  def maxWithVoidAsInfinity(i: Long, j: Long): Long = {
    if (i == Void) return j
    if (j == Void) return i
    return math.max(i,j)
  }

}
