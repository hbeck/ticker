package evaluation.diss

import core.lars._
import DissEvalHelpers._
import DissEvalPreparedAtoms._
import DissEvalPreparedVariables._

/**
  * Created by hb on 05.04.18.
  */
object DissEvalPrograms {

  val test = LarsProgram(Seq[LarsRule](rule(a,wt_D(10,b))))

  def baseWAt(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wt_At(windowSize,T,b))))
  def baseWD(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wt_D(windowSize,b))))
  def baseWB(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wt_B(windowSize,b))))

}
