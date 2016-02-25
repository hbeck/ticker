package asp

import core._
import aspsamples.SingleHusbandSample
import org.scalatest.{FlatSpec}

/**
  * Created by FM on 25.02.16.
  */
class AspSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")

  "An empty program" should "be executed and converted back to empty models" in {
    val program = Program()

    val asp = Asp(program)
    assert(asp == None)
  }

  "A program containing only a premise A" should "be executed an converted to one single model containing A" in {
    val program = Program(Premise(a))

    val asp = Asp(program)

    assert(asp.isDefined)
    assert(asp.get.contains(a))
  }

  "A program containing a premise and a rule" should "return only the premise" in {
    val program = Program(Premise(a), Rule.in(b).head(c))

    val asp = Asp(program)

    assert(asp.isDefined)
    assert(asp.get.isInstanceOf[SingleModel])
    assert(asp.get.contains(a))
  }
  it should "return two nodes" in {
    val program = Program(Premise(a), Rule.in(a).head(b))

    val asp = Asp(program)

    assert(asp.isDefined)
    assert(asp.get.contains(a))
  }

  "A program with two models" can "be executed and converted back into both models" in {
    val example = new SingleHusbandSample()
    val asp = Asp(example.program)

    assert(asp.get.isInstanceOf[MultipleModels])
    assert(asp.get contains Set(example.man, example.husband))
    assert(asp.get contains Set(example.man, example.single))
  }
}
