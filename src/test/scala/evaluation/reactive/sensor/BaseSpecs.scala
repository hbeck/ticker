package evaluation.reactive.sensor

import engine.config.BuildEngine
import org.scalatest.FlatSpec

import scala.util.Random

/**
  * Created by fm on 14/02/2017.
  */
class BaseSpecs extends FlatSpec with SensorScenario {

  "Sliding Time Window of length 1 and with all rules" should "have always the same yellows" in {
    val program = timeWindowProgram(1)

    val engine = BuildEngine.withProgram(program).configure().withClingo().use().usePull().start()

    val signals = continuousSignalStream(new Random(1))(100000)

    signals foreach {
      case (t, s) => {
        engine.append(t)(s)

        val model = engine.evaluate(t)

        model.get match {
          case Some(m) => {
            if (m.contains(yellow_1))
              assert(m contains yellow_2)
            else
              assert(!(m contains yellow_2))
          }
          case None =>
        }
      }

    }

  }
}
