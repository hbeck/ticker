package evaluation.diss.programs.traits

import reasoner.Result

trait NoVerification extends Verifiable {

  override def verifyOutput(result: Result, t: Int): Unit = {}

}
