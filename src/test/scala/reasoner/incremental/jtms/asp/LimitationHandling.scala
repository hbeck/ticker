package reasoner.incremental.jtms.asp

import core.Atom
import reasoner.incremental.jtms.ChoiceControl
import reasoner.incremental.jtms.algorithms.Jtms

/**
  * Created by hb on 5/31/16.
  */
object LimitationHandling {

  def assertModelWithKnownLimitation[T <: Jtms with ChoiceControl](tms: T, model: Set[Atom], knownLimitation: => Boolean): Unit = {
    assertModelWithKnownLimitation(tms, tms.getModel.get == model, knownLimitation)
  }

  def assertModelWithKnownLimitation[T <: Jtms with ChoiceControl](tms: T, modelCondition: => Boolean, knownLimitation: => Boolean): Unit = {
    if (tms.getModel == None) {
      if (!knownLimitation) { //known limitation
        println("rules: "+tms.rules)
        println("statusSeq: "+tms.statusSeq)
        println("choiceSeq: "+tms.choiceSeq)
        assert(false)
      }
    } else {
      assert(modelCondition)
    }
  }

}
