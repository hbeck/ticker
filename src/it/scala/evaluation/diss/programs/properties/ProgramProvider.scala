package evaluation.diss.programs.properties

import core.lars.LarsProgram

trait ProgramProvider {

  def program(): LarsProgram

}
