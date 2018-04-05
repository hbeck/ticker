package evaluation.diss

import core.lars._
import DissEvalHelpers._
import DissEvalPreparedAtoms._

/**
  * Created by hb on 05.04.18.
  */
object DissEvalPrograms {

  val testProgram = LarsProgram(Seq[LarsRule](rule(a,wD(10,b))))

}
