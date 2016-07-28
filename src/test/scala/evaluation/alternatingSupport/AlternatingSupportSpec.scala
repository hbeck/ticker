package evaluation.alternatingSupport

import fixtures.AtomTestFixture
import core.lars._

/**
  * Created by FM on 28.07.16.
  */
trait AlternatingSupportSpec extends AtomTestFixture {
  val rule_b = a <= W(10, Diamond, b)
  val rule_c = a <= W(10, Diamond, c)
  val rule_a = d <= a
  val program = LarsProgram.from(rule_b, rule_c, rule_a)
}
