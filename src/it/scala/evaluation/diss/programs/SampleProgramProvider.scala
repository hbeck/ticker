package evaluation.diss.programs

import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.PreparedAtoms.{a, b}

trait SampleProgramProvider extends AnalyticProgramProvider {

  def program(): LarsProgram = LarsProgram(Seq[LarsRule](rule(a,wt_D(10,b))))

}
