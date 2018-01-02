package reasoner.parser.factories

import core._
import reasoner.parser.wrapper.OperationWrapper
import org.scalatest.{BeforeAndAfterEach, FlatSpec}

/**
  * Created by et on 22.04.17.
  */
class OperationFactoryTest extends FlatSpec with BeforeAndAfterEach {

  private val wrapper = OperationWrapper(ArgumentFactory("A"),Some("+"),Some(ArgumentFactory(6)))
  private val argument1 = ArgumentFactory(10)
  private val argument2 = ArgumentFactory(11)
  private val operation = "="
  private val A: NumericArgument = "A"
  private val B: NumericArgument = "6"
  private val C: NumericArgument = "10"
  private val D: NumericArgument = "11"

  behavior of "OperationFactoryTest"

  it should "apply left hand operations" in {
    assert(OperationFactory(wrapper,operation,argument1).operation == Plus(A,B,C))
  }

  it should "apply right hand operations" in {
    assert(OperationFactory(argument1,operation,wrapper).operation == Plus(A,B,C))
  }

  it should "apply binary relations" in {
    assert(OperationFactory(argument1,"<",argument2).operation == Lt(C,D))
    assert(OperationFactory(argument2,">",argument1).operation == Gt(D,C))
    assert(OperationFactory(argument2,"!=",argument1).operation == Neq(D,C))
  }

}
