package engine

import core.{Atom, Program, ProgramBuilder, not}
import engine._
import org.scalatest.FlatSpec

import scala.collection.SortedMap

/**
  * Created by FM on 10.04.16.
  */
class EngineSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")

  val program = Program(
    a :- b,
    b :- c and not(d)
  )
  // TODO: figure out how to correctly name the atoms in the builder
  val programWithBuilder = ProgramBuilder({
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
      t1 -> EngineAtom("b"),
      t2 -> EngineAtom("a"),
      t3 -> EngineAtom("c")
    )

    def loadStreamFromFile = {
      Stream.fromItems(
        t2 -> Set(EngineAtom("a"), EngineAtom("d"))
      )
    }
    val stream_2 = loadStreamFromFile

    engine.add(stream_1)
    engine.add(stream_2)

    engine
  }

  "The Asp Evaluation" should "return a result for t1" in {
    val engine = engineWithStreams(AspEvaluation(program))

    assert(engine.evaluate(t1) == Set(EngineAtom("a"), EngineAtom("b")))
  }

  it should "invalidate 'b' for t3" in {
    val engine = engineWithStreams(AspEvaluation(program))


    assert(
      engine.evaluate(t3) == Set(EngineAtom("a"), EngineAtom("c"), EngineAtom("d")),
      "currently there is no way to 'outdate' the data in the intensional stream, therefor this test fails"
    )
  }
}

