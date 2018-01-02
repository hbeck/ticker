package reasoner.incremental.jtms

import core.Atom

/**
  * Created by hb on 6/10/16.
  */
trait ChoiceControl {

  var doForceChoiceOrder = false
  var forcedChoiceSeq = Seq[Atom]()

  def forceChoiceOrder(seq: Seq[Atom]) = {
    doForceChoiceOrder = true
    forcedChoiceSeq = seq
  }

  def removeChoiceOrder() = {
    doForceChoiceOrder = false
    forcedChoiceSeq = Seq[Atom]()
  }

}
