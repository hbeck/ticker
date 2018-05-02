package evaluation.diss.programs

import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.PreparedAtoms.{a, b}
import evaluation.diss.programs.traits.Analytic

trait SampleProgramProvider extends Analytic {

  def program(): LarsProgram = LarsProgram(Seq[LarsRule](rule(a,wt_D(10,b))))

}
