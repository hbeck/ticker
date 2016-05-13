package engine

import core.Atom
import core.asp.{AspFact, AspProgram}
import core.lars.{Time, TimePoint}
import org.scalatest.FlatSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 13.05.16.
  */
class TransformAspToTimePointSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")

  val t1: TimePoint = 1
  val t2: TimePoint = 2

  "An empty AspProgram with no dataStream at t1" should "contain only 'now(t1).'" in {
    val p = AspProgram()
    val dataStream: Stream = Set()

    val transformed = PinAspProgramToTimePoint(p, dataStream, t1)
    transformed.rules should contain(AspFact(now(t1)))
    transformed.timePoint should be(t1)
  }
  "An empty AspProgram with dataStream 't1 -> a' at t1" should "contain  'a(t1), now(t1).'" in {
    val p = AspProgram()
    val dataStream: Stream = Set(StreamEntry(t1, Set(a)))

    val transformed = PinAspProgramToTimePoint(p, dataStream, t1)

    transformed.rules should contain allOf(AspFact(a(t1)), AspFact(now(t1)))
  }

  "An empty AspProgram with dataStream 't1 -> {a,c}, t2 -> b' at t2" should "contain  'a(t1)., b(t2). now(t2).'" in {
    val p = AspProgram()
    val dataStream: Stream = Set(
      StreamEntry(t1, Set(a, c)),
      StreamEntry(t2, Set(b))
    )

    val transformed = PinAspProgramToTimePoint(p, dataStream, t2)

    transformed.rules should contain allOf(AspFact(a(t1)),AspFact(c(t1)), AspFact(b(t2)), AspFact(now(t2)))
  }
}
