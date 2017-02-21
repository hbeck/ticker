package evaluation.reactive.sensor

import core.{Model, PinnedAtom}
import core.lars.{LarsProgram, TimePoint, TimeWindowSize, TupleCount}
import engine.EvaluationEngine
import engine.config.BuildEngine
import org.scalatest.FlatSpec

import scala.util.Random

/**
  * Created by fm on 14/02/2017.
  */
class BaseSpecs extends FlatSpec with ExecuteHelper {

  val defaultProgramRunner = runWithProgram(new Random(1), 1000) _

  "Sliding Time Window of length 1 and with all rules" should "have always the same yellows" in {
    runClingoOneShot(defaultProgramRunner)(1, w => timeWindowProgram()(TimeWindowSize(w)))
  }

  "Sliding Time Window of length 2 and with all rules" should "have always the same yellows" in {
    runClingoOneShot(defaultProgramRunner)(2, w => timeWindowProgram()(TimeWindowSize(w)))
  }
  "Sliding Time Window of length 10 and with all rules" should "have always the same yellows" in {
    runClingoOneShot(defaultProgramRunner)(10, w => timeWindowProgram()(TimeWindowSize(w)))
  }
  "Sliding Time Window of length 100 and with all rules" should "have always the same yellows" in {
    runClingoOneShot(defaultProgramRunner)(100, w => timeWindowProgram()(TimeWindowSize(w)))
  }

  "Sliding Tuple Window of length 1 and with all rules" should "have always the same yellows" in {
    runClingoOneShot(defaultProgramRunner)(1, tupleWindowProgram())
  }


}
