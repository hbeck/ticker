package engine

import core.asp.{AspFact, AspProgram, AspProgramBuilder}
import core._
import core.lars.TimePoint
import engine.asp.{Direct, EvaluationStrategy}
import engine.asp.evaluation.StreamingAspInterpeter$
import engine.config.BuildEngine
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
    a :- b and d not c,
    b :- c not d
  )
  // TODO: figure out how to correctly name the atoms in the builder
  val programWithBuilder = AspProgramBuilder({
    case a #:: b #:: c #:: d #:: atoms => Set(
      a :- b,
      b :- c not d
    )
  })

  val t1 = TimePoint(1)
  val t2 = TimePoint(2)
  val t3 = TimePoint(3)

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

//  val e = BuildEngine.withProgram(program).useAsp().withClingo().use(Direct).usePull().start

  "The Asp Evaluation" should "return a result for t1" in {
    val engine = engineWithStreams(EvaluationStrategy.pull(program))
    val result = engine.evaluate(t1).get

    result.value should contain allOf(a(t2.toString), b(t1.toString))
  }

  it should "invalidate 'b' for t2" in {
    val engine = engineWithStreams(EvaluationStrategy.pull(program))

    val result = engine.evaluate(t2).get

    result.value should contain allOf(a(t2.toString), c(t2.toString), d(t2.toString))
  }
}

