package engine

import core.{ProgramBuilder, not}
import engine._
import org.scalatest.FlatSpec

import scala.collection.SortedMap

/**
  * Created by FM on 10.04.16.
  */
class EngineSpec extends FlatSpec {

  val program = ProgramBuilder({
    case a #:: b #:: c #:: d #:: atoms => Set(
      a :- b,
      b :- c and not(d)
    )
  })

  "The engine" should "work" in {
    val engine = Engine(AspEvaluation(program))

    val stream = Stream.fromItems(Minute(1) -> EngineAtom("a"), Minute(2) -> EngineAtom("b"), Minute(3) -> EngineAtom("c"))

    engine.add(stream)

    val v = engine.evaluate(Minute(1))

    assert(v == Set(EngineAtom("a")))
  }


}

case class Engine(private val evaluationEngine: EvaluationEngine) {

  val collector = new Collector

  var intentsional = SortedMap.empty[Time, Set[EngineAtom]](
    Ordering.fromLessThan((l, r) => l.milliseconds < r.milliseconds)
  )

  def add(observable: Observable) = {
    observable.subscribe(collector)
  }

  def evaluate(time: Time) = evaluationEngine.evaluate(time)

  class Collector extends Observer {
    def append(evaluation: Evaluation): Unit = {
      intentsional = intentsional.updated(evaluation.time, intentsional.getOrElse(evaluation.time, Set[Atom]()) ++ evaluation.atoms.toSet)

      evaluationEngine.append(evaluation.time)(evaluation.atoms.toSeq: _*)
    }
  }

}
