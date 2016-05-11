package lars.transform

import core.Atom
import engine.{TransformLars, Time}
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.05.16.
  */
class TransformLarsSpec extends FlatSpec {
  val t0 = Time(0)
  val t1 = Time(1)
  val t2 = Time(2)

  val T = TransformLars.T
  val now = TransformLars.now

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
}
