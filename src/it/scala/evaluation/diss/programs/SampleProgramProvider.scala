package evaluation.diss.programs

import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.Prepared.{a, b}
import evaluation.diss.programs.traits.AnalyticProgramProvider

trait SampleProgramProvider extends AnalyticProgramProvider {

  def program(): LarsProgram = LarsProgram(Seq[LarsRule](rule(a,wt_D(10,b))))

}
