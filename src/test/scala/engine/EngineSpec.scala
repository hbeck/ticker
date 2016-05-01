package engine

import core.{Atom, AspProgram, AspProgramBuilder, not}
import engine.implementations.AspEvaluation
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 10.04.16.
  */
class EngineSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")

  val program = AspProgram(
    a :- b,
    b :- c and not(d)
  )
  // TODO: figure out how to correctly name the atoms in the builder
  val programWithBuilder = AspProgramBuilder({
    case a #:: b #:: c #:: d #:: atoms => Set(
      a :- b,
      b :- c and not(d)
    )
  })

  val t1 = At.minute(1)
  val t2 = At.minute(2)
  val t3 = At.minute(3)

  def engineWithStreams(evaluationEngine: EvaluationEngine) = {
    val engine = Engine(evaluationEngine)

    val stream_1 = Stream.fromItem(
      t1 -> Atom("b"),
      t2 -> Atom("a"),
      t3 -> Atom("c")
    )

    def loadStreamFromFile = {
      Stream.fromItems(
        t2 -> Set(Atom("c"), Atom("d"))
      )
    }
    val stream_2 = loadStreamFromFile

    engine.add(stream_1)
    engine.add(stream_2)

    engine
  }

  "The Asp Evaluation" should "return a result for t1" in {
    val engine = engineWithStreams(AspEvaluation.pull(program))
    val result = engine.evaluate(t1).get

    result.value should contain allOf(a, b)
  }

  it should "invalidate 'b' for t3" in {
    val engine = engineWithStreams(AspEvaluation.pull(program))

    val result = engine.evaluate(t2).get

    result.value should contain allOf(a, c, d)
  }
}

