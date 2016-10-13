package jtms.asp

import core.Atom
import jtms.{ChoiceControl, Jtms, JtmsUpdateAlgorithm}

/**
  * Created by hb on 5/31/16.
  */
object LimitationHandling {

  def assertModelWithKnownLimitation[T <: JtmsUpdateAlgorithm with ChoiceControl](tms: T, model: Set[Atom], knownLimitation: => Boolean): Unit = {
    assertModelWithKnownLimitation(tms, tms.getModel.get == model, knownLimitation)
  }

  def assertModelWithKnownLimitation[T <: JtmsUpdateAlgorithm with ChoiceControl](tms: T, modelCondition: => Boolean, knownLimitation: => Boolean): Unit = {
    if (tms.getModel == None) {
      if (!knownLimitation) { //known limitation
        println("rules: "+tms.jtms.rules)
        println("statusSeq: "+tms.statusSeq)
        println("choiceSeq: "+tms.choiceSeq)
        assert(false)
      }
    } else {
      assert(modelCondition)
    }
  }

}
