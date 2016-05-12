package lars.transform

import core.Atom
import core.lars.TimePoint
import engine.{PlainLarsToAsp}
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.05.16.
  */
class TransformLarsSpec extends FlatSpec {
  val t0 = TimePoint(0)
  val t1 = TimePoint(1)
  val t2 = TimePoint(2)

  val T = core.lars.T
  val now = PlainLarsToAsp.now

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
}
