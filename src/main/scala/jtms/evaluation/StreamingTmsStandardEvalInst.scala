package jtms.evaluation

import core.asp.NormalRule

/**
  * Created by hb on 11.04.17.
  */
trait StreamingTmsStandardEvalInst extends StreamingTmsEvalInst {

  def windowSize: Int

  def rulesToAddAt(t: Int) = immediatelyExpiringRulesFor(t) ++ rulesExpiringAfterWindow(t)
  def rulesToRemoveAt(t: Int) = immediatelyExpiringRulesFor(t-1) ++ rulesExpiringAfterWindow(t - windowSize - 1)

  def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule]
  def rulesExpiringAfterWindow(t: Int): Seq[NormalRule]
  def generateFactsToAddAt(t: Int): Seq[NormalRule]

  var addedFacts = Map[Int,Seq[NormalRule]]()

  def factsToAddAt(t: Int): Seq[NormalRule] = {
    val rules = generateFactsToAddAt(t)
    addedFacts = addedFacts + (t -> rules)
    rules
  }

  def factsToRemoveAt(t: Int): Seq[NormalRule] = {
    val u = t - windowSize - 1
    addedFacts.get(u) match {
      case Some(seq) => {
        addedFacts = addedFacts - u
        seq
      }
      case None => Seq()
    }
  }

}
