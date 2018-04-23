package evaluation.diss.programs

import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.instances.AnalyticCommon
import evaluation.diss.Helpers._
import evaluation.diss.PreparedAtoms.{a, b}

trait BasicProgramProvider extends AnalyticProgramProvider {

  def program(): LarsProgram = {
    val windowAtom = AnalyticCommon.windowAtom(winMod,windowSize,b)
    LarsProgram(Seq[LarsRule](rule(a,windowAtom)))
  }

}
