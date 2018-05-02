package evaluation.diss.programs

import core.lars.LarsProgram
import evaluation.diss.PreparedAtoms.{a, b}
import evaluation.diss.programs.traits.Analytic.makeWindowAtom
import evaluation.diss.programs.traits.Analytic

trait BasicProgramProvider extends Analytic {

  def program(): LarsProgram = {
    val windowAtom = makeWindowAtom(winMod,windowSize,b)
    LarsProgram.from(a <= windowAtom)
  }

}
