package engine.asp.tms

import core.asp.NormalRule
import engine.asp._

/**
  * Created by hb on 22.04.17.
  */
trait AnnotatedNormalRule {
  val rule: NormalRule
}

case class StaticNormalRule(rule: NormalRule) extends AnnotatedNormalRule

trait NormalRuleWithDuration extends AnnotatedNormalRule {
  val duration: TickDuration
  val expirationMode: ExpirationMode
  val preparationMode: PreparationMode
}

case class NormalRuleWithTimeDuration(rule: NormalRule, duration: TickDuration, expirationMode: ExpirationMode, preparationMode: PreparationMode = MayBePregrounded) extends NormalRuleWithDuration
case class NormalRuleWithCountDuration(rule: NormalRule, duration: TickDuration, expirationMode: ExpirationMode, preparationMode: PreparationMode = MayBePregrounded) extends NormalRuleWithDuration
case class NormalRuleWithDualDuration(rule: NormalRule, duration: TickDuration, expirationMode: ExpirationMode, preparationMode: PreparationMode = MayBePregrounded) extends NormalRuleWithDuration

trait ExpiringNormalRule extends AnnotatedNormalRule {
  val deadline: Tick
  val expirationMode: ExpirationMode
}

case class NormalRuleTimeExpiration(rule: NormalRule, deadline: Tick, expirationMode: ExpirationMode) extends ExpiringNormalRule //necessarily ground
case class NormalRuleCountExpiration(rule: NormalRule, deadline: Tick, expirationMode: ExpirationMode) extends ExpiringNormalRule //necessarily ground
case class NormalRuleDualExpiration(rule: NormalRule, deadline: Tick, expirationMode: ExpirationMode) extends ExpiringNormalRule //necessarily ground

sealed trait ExpirationMode
case object ExpirationObligatory extends ExpirationMode
case object ExpirationOptional extends ExpirationMode

sealed trait PreparationMode
case object NeedsIncrementalGrounding extends PreparationMode
case object MayBePregrounded extends PreparationMode //grounding apart from pin-variables is possible (unless values are not known upfront)

//rule that *must* be deleted after ticksUntilExpired (resp. its groundings)
//rule that *can* be deleted after ticksUntilOutdated (resp. its groundings)
//case class NormalRuleWithOutdatingDuration(rule: NormalRule, duration: TickDuration) extends NormalRuleWithDuration
//case class OutdatingNormalRule(rule: NormalRule, deadline: Tick) extends VolatileNormalRule //necessarily ground

