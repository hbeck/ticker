package evaluation.diss.instances

import core.Atom
import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers.{rule, wt_D}
import evaluation.diss.Instance
import evaluation.diss.PreparedAtoms.{a,b}
import reasoner.Result

/**
  * Created by hb on 05.04.18.
  */
case class SampleInstance() extends Instance {

  def program = LarsProgram(Seq[LarsRule](rule(a,wt_D(10,b))))

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
