package engine.asp.tms

import core.asp.NormalRule
import engine.asp._

/**
  * Created by hb on 22.04.17.
  */
trait AnnotatedNormalRule {
  val rule: NormalRule
}

case class StaticRule(rule: NormalRule) extends AnnotatedNormalRule

trait RuleWithDuration extends AnnotatedNormalRule {
  val duration: TickDuration
  val expirationMode: ExpirationMode
  val generationMode: GenerationMode
}

trait RuleWithTimeDuration extends RuleWithDuration
trait RuleWithCountDuration extends RuleWithDuration

case class RuleWithTimeDurationOnly(rule: NormalRule, duration: TickDuration, expirationMode: ExpirationMode, generationMode: GenerationMode = OnTimeIncreaseOnly) extends RuleWithTimeDuration
case class RuleWithCountDurationOnly(rule: NormalRule, duration: TickDuration, expirationMode: ExpirationMode, generationMode: GenerationMode = OnCountIncreaseOnly) extends RuleWithCountDuration
case class RuleWithDualDuration(rule: NormalRule, duration: TickDuration, expirationMode: ExpirationMode, generationMode: GenerationMode = OnTimeAndCountIncrease) extends RuleWithTimeDuration with RuleWithCountDuration

trait ExpiringRule extends AnnotatedNormalRule {
  val expiration: Tick
  val expirationMode: ExpirationMode
}

trait RuleExpiringByTime extends ExpiringRule
trait RuleExpiringByCount extends ExpiringRule

case class RuleExpiringByTimeOnly(rule: NormalRule, expiration: Tick, expirationMode: ExpirationMode) extends RuleExpiringByTime //necessarily ground
case class RuleExpiringByCountOnly(rule: NormalRule, expiration: Tick, expirationMode: ExpirationMode) extends RuleExpiringByCount //necessarily ground
case class RuleExpiringDually(rule: NormalRule, expiration: Tick, expirationMode: ExpirationMode) extends RuleExpiringByTime with RuleExpiringByCount //necessarily ground

sealed trait ExpirationMode
case object ExpirationObligatory extends ExpirationMode
case object ExpirationOptional extends ExpirationMode

sealed trait GenerationMode
case object OnTimeIncreaseOnly extends GenerationMode
case object OnCountIncreaseOnly extends GenerationMode
case object OnTimeAndCountIncrease extends GenerationMode
