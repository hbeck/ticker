package evaluation.diss.programs.traits

import reasoner.Result

trait Verifiable {

  def verifyOutput(result: Result, t: Int): Unit

}
