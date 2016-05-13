package engine

import core.asp.AspProgram
import core.lars.AtTime
import core.Atom
import engine.asp.{AspPullEvaluationEngine, EvaluationStrategy}
import engine.asp.evaluation.AspEvaluation
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 21.04.16.
  */
class EngineStreamSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")

  val program = AspProgram(
    a :- b,
    b :- c  not d
  )

  val t1 = AtTime.second(1)
  val t2 = AtTime.second(2)
  val t3 = AtTime.second(3)

  def evaluationEngine: EvaluationEngine = EvaluationStrategy.pull(program)

  "Adding atoms one after another at the same timepoint" should "lead to different evaluation results" in {
    val engine = evaluationEngine

    val atT1 = engine.append(t1) _

    atT1(Seq(Atom("c")))

    assume(Set(a(t1.toString), b(t1.toString), c(t1.toString)) subsetOf engine.evaluate(t1).get.value)

    atT1(Seq(Atom("d")))

    engine.evaluate(t1).get.value should contain allOf(c(t1.toString), d(t1.toString))
  }

  "Adding one atom at t2" should "not lead to a result at t3" in {
    val engine = evaluationEngine

    engine.append(t2)(Atom("c"))

    assume(Set(a, b, c).subsetOf(engine.evaluate(t2).get.value))

    pendingUntilFixed {
      assert(engine.evaluate(t3).get.isEmpty)
    }
  }

  it should "not lead to a result when evaluating at t1" in {
    val engine = evaluationEngine

    engine.append(t2)(Atom("c"))

    assume(Set(a, b, c) subsetOf engine.evaluate(t2).get.value)

    pendingUntilFixed {
      assert(engine.evaluate(t1).get.isEmpty)
    }
  }
}
