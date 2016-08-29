package lars

import core.lars.{Diamond, LarsProgram, LarsRule, W}
import engine.asp.{LarsToPinnedProgram, SanitizeLarsProgram}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 12.08.16.
  */
class SanitizeProgramSpec extends FlatSpec with TimeTestFixtures {

  val rule: LarsRule = a <= b
  val program = LarsProgram.from(rule)

  val sanitized = SanitizeLarsProgram(program)


  "In the rule 'a <= b'" should "treat b as extensional atom" in {
    sanitized.extensionalAtoms should contain only b
  }

  it should "treat a as intensional" in {
    sanitized.intensionalAtoms should contain only a
  }

  it should "sanitize b as w^0 D b " in {
    sanitized.sanitize(b) should be((W(0, Diamond, b)))
  }

  "A rule with only intensional atoms in the body" should "remain unchanged" in {
    val rule2 = b <= c
    val p2 = LarsProgram.from(rule, rule2)
    val sanitizer = SanitizeLarsProgram(p2)
    sanitizer.sanitize(rule) should equal(rule)
  }

}

