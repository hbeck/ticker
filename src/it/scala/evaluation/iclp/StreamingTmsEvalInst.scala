package evaluation.iclp

import core.asp.NormalRule

/**
  * Created by hb on 04.04.17.
  */
trait StreamingTmsEvalInst extends LarsEvaluationInstance {

  val windowSize: Int

  //<manual TMS>
  def staticRules(): Seq[NormalRule]

  var addedFacts = Map[Int,Seq[NormalRule]]()

  def manualTmsFactsToAddAt(t: Int): Seq[NormalRule] = {
    val rules = generateFactsToAddAt(t)
    addedFacts = addedFacts + (t -> rules)
    rules
  }
  def manualTmsFactsToRemoveAt(t: Int): Seq[NormalRule] = {
    val u = t - windowSize - 1
    addedFacts.get(u) match {
      case Some(seq) => {
        addedFacts = addedFacts - u
        seq
      }
      case None => Seq()
    }
  }

  def manualTmsRulesToAddAt(t: Int) = immediatelyExpiringRulesFor(t) ++ rulesExpiringAfterWindow(t)
  def manualTmsRulesToRemoveAt(t: Int) = immediatelyExpiringRulesFor(t-1) ++ rulesExpiringAfterWindow(t - windowSize - 1)

  def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule]
  def rulesExpiringAfterWindow(t: Int): Seq[NormalRule]

  def generateFactsToAddAt(t: Int): Seq[NormalRule] = generateSignalsToAddAt(t) map (pinnedFact(_,t))
  //</manual TMS>




}


