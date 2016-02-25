package asp

import core.{Rule, Premise, Atom, Program}
import aspsamples.SingleHusbandSample
import org.scalatest.{FlatSpec}

/**
  * Created by FM on 25.02.16.
  */
class AspSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")

  "An empty program" can "be executed and converted back to empty models" in {
    val program = Program()

    val asp = Asp(program)
    assert(asp == Set())
  }

  "A program containing only a premise A" can "be executed an converted to one single model containing A" in {
    val program = Program(Premise(a))

    val asp = Asp(program)

    assert(asp.size == 1)
    assert(asp.head == Set(a))
  }

  "A program containing a premise and a rule" should "return only the premise" in {
    val program = Program(Premise(a), Rule.in(b).head(c))

    val asp = Asp(program)

    assert(asp.size == 1)
    assert(asp.head.size == 1)
  }
  it should "return two nodes" in {
    val program = Program(Premise(a), Rule.in(a).head(b))

    val asp = Asp(program)

    assert(asp.size == 1)
    assert(asp.head.size == 2)
  }

  "A program with two models" can "be executed and converted back into both models" in {
    val example = new SingleHusbandSample()
    val asp = Asp(example.program)

    assert(asp.size == 2)
    assert(asp contains Set(example.man, example.husband))
    assert(asp contains Set(example.man, example.single))
  }
}
