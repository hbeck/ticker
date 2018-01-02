package reasoner.parser.factories

import core.Argument
import reasoner.parser.InvalidSyntaxException
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class ArgumentFactoryTest extends FlatSpec {

  behavior of "ArgumentFactoryTest"

  it should "create Arguments from strings and numbers" in {
    assert(ArgumentFactory("A").arg == Argument.convertToArgument("A"))
    assert(ArgumentFactory(42).arg == Argument.convertToArgument("42"))
  }

  it should "fail for non strings and non numbers" in {
    intercept[InvalidSyntaxException] {
      ArgumentFactory('c').arg
    }
  }
}
