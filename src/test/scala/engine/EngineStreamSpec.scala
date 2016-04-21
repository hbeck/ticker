package engine

import core.{Atom, Program, not}
import org.scalatest.FlatSpec

/**
  * Created by FM on 21.04.16.
  */
class EngineStreamSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")

  val program = Program(
    a :- b,
    b :- c and not(d)
  )

  val t1 = At.minute(1)
  val t2 = At.minute(2)
  val t3 = At.minute(3)

  def evaluationEngine: EvaluationEngine = AspEvaluation(program)

  "Adding atoms one after another at the same timepoint" should "lead to different evaluation results" in {
    val engine = evaluationEngine

    val atT1 = engine.append(t1) _

    atT1(Seq(Atom("c")))

    assume(engine.evaluate(t1).value contains Set(a, b, c))

    atT1(Seq(Atom("d")))

    assert(engine.evaluate(t1).value contains Set(c, d))
  }

  "Adding one atom at t2" should "not lead to a result at t3" in {
    val engine = evaluationEngine

    engine.append(t2)(Atom("c"))

    assume(engine.evaluate(t2).value contains Set(a, b, c))

    assert(engine.evaluate(t3).value.isEmpty)
  }

  it should "not lead to a result when evaluating at t1" in {
    val engine = evaluationEngine

    engine.append(t2)(Atom("c"))

    assume(engine.evaluate(t2).value contains Set(a, b, c))

    assert(engine.evaluate(t1).value.isEmpty)
  }
}
