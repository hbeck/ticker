package engine

import core.Atom
import core.asp.{AspFact, AspProgram}
import core.lars.{TimePoint, Time}
import org.scalatest.FlatSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 13.05.16.
  */
class TransformAspToTimePointSpec extends FlatSpec {

  val a = Atom("a")

  val t1: TimePoint = 1

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
}
