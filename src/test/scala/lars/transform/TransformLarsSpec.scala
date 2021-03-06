package lars.transform

import core.asp.NormalRule
import core.lars.{TimeWindow, WindowAtom}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import reasoner.common.{PlainLarsToAspMapper, WindowAtomEncoder}

import scala.concurrent.duration._

/**
  * Created by FM on 05.05.16.
  */
class TransformLarsSpec extends FlatSpec with TimeTestFixtures {

  val now = reasoner.now
  val cnt = reasoner.cnt

  val st1 = TimeWindow(1)

  def DefaultLarsToPinnedProgram: PlainLarsToAspMapper = PlainLarsToAspMapper(1 second)

  def allWindowRules(encoder: WindowAtomEncoder): Seq[NormalRule] = encoder.allWindowRules

  def allWindowRules(windowAtom: WindowAtom): Seq[NormalRule] = allWindowRules(DefaultLarsToPinnedProgram.windowAtomEncoder(windowAtom))
}
