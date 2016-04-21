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

  def evaluationEngine: EvaluationEngine = AnswerUpdateNetworkEngine(program)

  "Adding atoms one after another at the same timepoint" should "lead to different evaluation results" in {
    val engine = evaluationEngine

    val atT1 = engine.append(t1) _

    atT1(Seq(EngineAtom("c")))

    assume(engine.evaluate(t1) == Set(EngineAtom("a"), EngineAtom("b"), EngineAtom("c")))

    atT1(Seq(EngineAtom("d")))

    assert(engine.evaluate(t1) == Set(EngineAtom("c"), EngineAtom("d")))
  }

  "Adding one atom at t2" should "not lead to a result at t3" in {
    val engine = evaluationEngine

    engine.append(t2)(EngineAtom("c"))

    assume(engine.evaluate(t2) == Set(EngineAtom("a"), EngineAtom("b"), EngineAtom("c")))

    assert(engine.evaluate(t3) == Set())
  }

  it should "not lead to a result when evaluating at t1" in {
    val engine = evaluationEngine

    engine.append(t2)(EngineAtom("c"))

    assume(engine.evaluate(t2) == Set(EngineAtom("a"), EngineAtom("b"), EngineAtom("c")))

    assert(engine.evaluate(t1) == Set())
  }
}
