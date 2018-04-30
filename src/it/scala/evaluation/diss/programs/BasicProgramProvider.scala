package evaluation.diss.programs

import core.lars.LarsProgram
import evaluation.diss.PreparedAtoms.{a, b}
import evaluation.diss.programs.AnalyticProgramProvider.makeWindowAtom

trait BasicProgramProvider extends AnalyticProgramProvider {

  def program(): LarsProgram = {
    val windowAtom = makeWindowAtom(winMod,windowSize,b)
    LarsProgram.from(a <= windowAtom)
  }

}
