package evaluation.diss.programs

import core.lars.LarsProgram
import evaluation.diss.instances.AnalyticCommon.WindowModalityCombi

trait AnalyticProgramProvider extends ProgramProvider {

  def program: LarsProgram
  def winMod: WindowModalityCombi
  def windowSize: Int

}
