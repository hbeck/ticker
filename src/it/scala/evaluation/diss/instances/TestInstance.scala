package evaluation.diss.instances

import core.Atom
import core.lars.LarsProgram
import evaluation.diss.DissEvalPreparedAtoms._
import evaluation.diss.{DissEvalInstance, DissEvalPrograms}
import reasoner.Result

/**
  * Created by hb on 05.04.18.
  */
case class TestInstance() extends DissEvalInstance {

  def program: LarsProgram = DissEvalPrograms.testProgram

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t % 20 >= 0 && t % 20 <= 5) {
      Seq[Atom](b)
    } else {
      Seq()
    }
  }

  def verifyOutput(result: Result, t: Int): Unit = {
    val model = result.model
    if (t % 20 >= 0 && t % 20 <= 15) {
      assert(model.contains(a))
    } else {
      if (model.contains(a)) {
        println("result at "+t+": "+model)
      }
      assert(!model.contains(a))
    }
  }

}
