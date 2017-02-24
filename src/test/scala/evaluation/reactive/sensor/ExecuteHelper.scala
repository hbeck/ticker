package evaluation.reactive.sensor

import core.{Model, PinnedAtom}
import core.lars.{LarsProgram, TimePoint}
import engine.{EvaluationEngine, StreamEntry}
import engine.config.BuildEngine
import org.scalatest.{FlatSpec, FlatSpecLike}

import scala.util.Random


/**
  * Created by fm on 21/02/2017.
  */
trait ExecuteHelper extends SensorScenario with FlatSpecLike {

  type EngineExecutor = (Long, => EvaluationEngine) => Unit

  def runClingoOneShot(defaultProgramRunner: EngineExecutor)(windowLength: Long, buildProgram: (Long) => LarsProgram) {
    val program = buildProgram(windowLength)

    val engine = BuildEngine.withProgram(program).configure().withClingo().use().usePull().start()

    defaultProgramRunner(windowLength, engine)
  }

  def runClingoReactive(defaultProgramRunner: EngineExecutor)(windowLength: Long, buildProgram: (Long) => LarsProgram) = {
    val program = buildProgram(windowLength)

    val engine = BuildEngine.withProgram(program).configure().withReactive().start()

    defaultProgramRunner(windowLength, engine)
  }


  def runWithProgram(random: Random, sampleSize: Int)(windowLength: Long, engineBuilder: => EvaluationEngine) = {
    val engine = engineBuilder

    val signals = continuousSignalStream(random)(sampleSize)

    val asserter = assertModel(windowLength) _

    signals foreach {
      case StreamEntry(t, atoms) => {
        engine.append(t)(atoms.toSeq: _*)

        val model = engine.evaluate(t)


        model.get match {
          // TODO
          case Some(m) if t.value >= windowLength + 1 => {
            asserter(t, m)

            if (t.value % 100 == 0)
              println(m)
          }
          case _ =>
        }
      }
    }
  }


  def assertModel(windowLength: Long)(now: TimePoint, model: Model) = {
    if (model.contains(yellow_1))
      assert(model contains yellow_2)
    else
      assert(!(model contains yellow_2))

    (0 to windowLength.toInt) map (now - _) foreach { time =>

      if (model.contains(PinnedAtom(med_1, time)))
        assert(model contains PinnedAtom(med_2, time))
      else
        assert(!(model contains PinnedAtom(med_2, time)))

    }

    assert((model contains green) | (model contains yellow_1) | (model contains warn))
  }
}
