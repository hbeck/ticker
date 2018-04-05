package evaluation.diss

import core.Atom
import core.lars.LarsProgram
import reasoner.Result

/**
  * Created by hb on 05.04.18.
  */
trait DissEvalInstance {

  def program: LarsProgram
  def generateSignalsToAddAt(t: Int): Seq[Atom]
  def verifyOutput(result: Result, t: Int): Unit

}
