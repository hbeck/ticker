package asp

import core._
import jtms.asp.examples.SingleHusbandSample
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
    assert(asp.isEmpty)
  }

  "A program containing only a premise A" should "be executed an converted to one single model containing A" in {
    val program = Program(Fact(a))

    val asp = Asp(program)

    assert(asp.nonEmpty)
    assert(asp contains Set(a))
  }

  "A program containing a premise and a rule" should "return only the premise" in {
    val program = Program(Fact(a), Rule.pos(b).head(c))

    val asp = Asp(program)

    assert(asp.nonEmpty)
    assert(asp.size == 1)
    assert(asp contains Set(a))
  }
  it should "return two nodes" in {
    val program = Program(Fact(a), Rule.pos(a).head(b))

    val asp = Asp(program)

    assert(asp.nonEmpty)
    assert(asp.size == 1)
    assert(asp contains Set(a, b))
  }

  "A program with two models" can "be executed and converted back into both models" in {
    val example = new SingleHusbandSample()
    val asp = Asp(example.pHusbandFirst)

    assert(asp.size == 2)
    assert(asp contains Set(example.man, example.husband))
    assert(asp contains Set(example.man, example.single))
  }
}
