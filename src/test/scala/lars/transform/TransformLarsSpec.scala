package lars.transform

import core.Atom
import core.lars.{SlidingTimeWindow, Time, TimePoint}
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.05.16.
  */
class TransformLarsSpec extends FlatSpec {
  val t0: Time = 0
  val t1: Time = 1
  val t2: Time = 2

  val T = core.lars.T
  val now = engine.asp.now

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")

  val st1 = SlidingTimeWindow(1)
}
