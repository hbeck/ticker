package reasoner.incremental

import core.Atom
import reasoner.common.Tick

class TickIncrementer(val reasoner: IncrementalReasoner) {

  val expiration = reasoner.expiration
  val jtms = reasoner.jtms
  val incrementalRuleMaker = reasoner.incrementalRuleMaker

  def incrementTick(currentTick: Tick, signal: Option[Atom] = None) {
    val timeIncrease = signal.isEmpty

    val expiredRules = if (timeIncrease) {
      expiration.expiringRulesAtTimeIncrement()
    } else {
      expiration.expiringRulesAtCountIncrement()
    }

    expiredRules.foreach(jtms.remove(_))

    val annotatedRules: Seq[ExpiringRule] = incrementalRuleMaker.incrementalRules(currentTick, signal)
    annotatedRules foreach {
      annotatedRule => {
        jtms.add(annotatedRule.rule)
        expiration.registerExpiration(annotatedRule)
      }
    }

    //println("tick "+currentTick+""+(if (signal.isDefined) ": "+signal.get))
    //println(jtms.getModel())
  }

}
