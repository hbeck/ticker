package evaluation.diss.programs

import core.lars.LarsProgram
import evaluation.diss.Prepared.{a, b}
import evaluation.diss.programs.properties.AnalyticProgramProvider.makeWindowAtom
import evaluation.diss.programs.properties.AnalyticProgramProvider

trait BasicProgramProvider extends AnalyticProgramProvider {

  def program(): LarsProgram = {
    val windowAtom = makeWindowAtom(winMod,windowSize,b)
    LarsProgram.from(a <= windowAtom)
  }

}
