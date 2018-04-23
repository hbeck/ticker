package evaluation.diss.programs

import core.lars.LarsProgram

trait ProgramProvider {

  def program(): LarsProgram

}
