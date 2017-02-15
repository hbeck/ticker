package evaluation.reactive.sensor

import core.{Model, PinnedAtom}
import core.lars.TimePoint
import engine.EvaluationEngine
import engine.config.BuildEngine
import org.scalatest.FlatSpec

import scala.util.Random

/**
  * Created by fm on 14/02/2017.
  */
class BaseSpecs extends FlatSpec with SensorScenario {

  val defaultProgramRunner = runWithProgram(new Random(1), 1000) _

  "Sliding Time Window of length 1 and with all rules" should "have always the same yellows" in {
    val windowLength = 1

    val program = timeWindowProgram(windowLength)

    val engine = BuildEngine.withProgram(program).configure().withClingo().use().usePull().start()

    defaultProgramRunner(windowLength, engine)
  }

  "Sliding Tuple Window of length 1 and with all rules" should "have always the same yellows" in {
    val windowLength = 1

    val program = tupleWindowProgram(windowLength)

    val engine = BuildEngine.withProgram(program).configure().withClingo().use().usePull().start()

    defaultProgramRunner(windowLength, engine)
  }

  def runWithProgram(random: Random, sampleSize: Int)(windowLength: Int, engineBuilder: => EvaluationEngine) = {
    val engine = engineBuilder

    val signals = continuousSignalStream(random)(sampleSize)

    val asserter = assertModel(windowLength) _

    signals foreach {
      case (t, s) => {
        engine.append(t)(s)

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


  def assertModel(windowLength: Int)(now: TimePoint, model: Model) = {
    if (model.contains(yellow_1))
      assert(model contains yellow_2)
    else
      assert(!(model contains yellow_2))

    (0 to windowLength) map (now - _) foreach { time =>

      if (model.contains(PinnedAtom(med_1, time)))
        assert(model contains PinnedAtom(med_2, time))
      else
        assert(!(model contains PinnedAtom(med_2, time)))

    }

    assert((model contains green) | (model contains yellow_1) | (model contains warn))
  }
}
