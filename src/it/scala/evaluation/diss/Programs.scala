package evaluation.diss

import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers.{rule, wt_D, _}
import evaluation.diss.PreparedAtoms.{a, b}
import evaluation.diss.PreparedVariables._

/**
  * Created by hb on 05.04.18.
  */
object Programs {

  val test = LarsProgram(Seq[LarsRule](rule(a,wt_D(10,b))))

  def baseWtAt(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wt_At(windowSize,T,b))))
  def baseWtD(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wt_D(windowSize,b))))
  def baseWtB(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wt_B(windowSize,b))))
  def baseWcAt(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wc_At(windowSize,T,b))))
  def baseWcD(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wc_D(windowSize,b))))
  def baseWcB(windowSize: Int) = LarsProgram(Seq[LarsRule](rule(a,wc_B(windowSize,b))))

}
