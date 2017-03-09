package engine.asp

/**
  * Created by hb on 05.03.17.
  *
  * Special logic to introduce irrelevance/infinity with -1
  *
  */
case class TickPair(time: Long, count: Long) {
  def + (other: TickPair): TickPair = {
    //ignoring -1 as number
    def add(n1: Long, n2: Long) = {
      if (n1 == Void || n2 == Void) Void
      else n1 + n2
    }
    TickPair(add(this.time,other.time),add(this.count,other.count))
  }

  def incrementTime(): TickPair = TickPair(time+1,count)
  def incrementCount(): TickPair = TickPair(time,count+1)
}

object TickPair {

  def min(p1: TickPair, p2: TickPair) = {
    val time = minWithVoidAsInfinity(p1.time,p2.time)
    val count = minWithVoidAsInfinity(p1.count,p2.count)
    TickPair(time,count)
  }

  def minWithVoidAsInfinity(i: Long, j: Long): Long = {
    if (i == Void) return j
    if (j == Void) return i
    return math.min(i,j)
  }

}
