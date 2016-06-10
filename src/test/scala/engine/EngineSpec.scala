package engine

import core._
import core.asp.{AspProgram, AspProgramBuilder}
import core.lars.LarsProgram
import engine.asp.{Direct, EvaluationStrategy}
import engine.config.BuildEngine
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 10.04.16.
  */
class EngineSpec extends FlatSpec with TimeTestFixtures {

  val program = LarsProgram.from(
    a <= b and d not c,
    b <= c not d
  )
  // TODO: figure out how to correctly name the atoms in the builder
  val programWithBuilder = AspProgramBuilder({
    case a #:: b #:: c #:: d #:: atoms => Set(
      a :- b,
      b :- c not d
    )
  })

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

    val engineConfig = BuildEngine.withProgram(program).useAsp().withClingo().use(Direct).usePull()

  "The Asp Evaluation" should "return a result for t1" in {
    val engine = engineWithStreams(engineConfig.start())
    val result = engine.evaluate(t1).get

    result.value should contain only b
  }

  it should "invalidate 'b' for t2" in {
    val engine = engineWithStreams(engineConfig.start())

    val result = engine.evaluate(t2).get

    result.value should contain allOf(a, c, d)
  }
}

