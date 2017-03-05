package engine.asp

/**
  * Created by hb on 05.03.17.
  *
  * Special logic to introduce irrelevance/infinity with -1
  *
  */
case class TickPair(time: Long, count: Long) {
  def + (other: TickPair): TickPair = {
    def add(n1: Long, n2: Long) = {
      if (n1 == -1) n2
      else if (n2 == -1) this.time
      else n1 + n2
    }
    TickPair(add(this.time,other.time),add(this.count,other.count))
  }
}

object TickPair {

  def min(p1: TickPair, p2: TickPair) = {
    val time = minWithMinusOneAsInfinity(p1.time,p2.time)
    val count = minWithMinusOneAsInfinity(p1.count,p2.count)
    TickPair(time,count)
  }

  def minWithMinusOneAsInfinity(i: Long, j: Long): Long = {
    if (i == -1) return j
    if (j == -1) return i
    return math.min(i,j)
  }

}
