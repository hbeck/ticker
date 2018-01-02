package lars.transform

import core.asp.NormalRule
import core.lars.{SlidingTimeWindow, WindowAtom}
import reasoner.asp.{PlainLarsToAspMapper, WindowAtomEncoder}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec

import scala.concurrent.duration._

/**
  * Created by FM on 05.05.16.
  */
class TransformLarsSpec extends FlatSpec with TimeTestFixtures {

  val now = reasoner.asp.now
  val cnt = reasoner.asp.cnt

  val st1 = SlidingTimeWindow(1)

  def DefaultLarsToPinnedProgram: PlainLarsToAspMapper = reasoner.asp.PlainLarsToAspMapper(1 second)

  def allWindowRules(encoder: WindowAtomEncoder): Seq[NormalRule] = encoder.allWindowRules

  def allWindowRules(windowAtom: WindowAtom): Seq[NormalRule] = allWindowRules(DefaultLarsToPinnedProgram.windowAtomEncoder(windowAtom))
}
