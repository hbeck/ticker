package reasoner.parser.factories

import reasoner.parser.InvalidSyntaxException
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class RuleFactoryTest extends FlatSpec {

  private val not = true

  private val a = AtomFactory(!not,"a",List())
  private val b = AtomFactory(!not,"b",List())
  private val c = AtomFactory(!not,"c",List())
  private val not_a = AtomFactory(not,"a",List())
  private val not_b = AtomFactory(not,"b",List())
  private val a_params = AtomFactory(!not,"a",List(10,"A","B"))
  private val c_params = AtomFactory(!not,"a",List("A","B",42))

  behavior of "RuleFactoryTestf"

  it should "create the rule with a head only" in {
    RuleFactory(a,List())
  }

  it should "create the rule with a head only with atoms with parameters" in {
    RuleFactory(a_params,List())
  }

  it should "create the rule" in {
    RuleFactory(a,List(b,c))
  }

  it should "create the rule with atoms with parameters" in {
    RuleFactory(a_params,List(b,c_params,not_b))
  }

  it should "not create the rule with a negtive head" in {
    intercept[InvalidSyntaxException] {
      RuleFactory(not_a, List()).ruleHead
    }
  }
}
