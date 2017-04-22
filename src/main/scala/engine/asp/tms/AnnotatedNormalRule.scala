package engine.asp.tms

import core.asp.NormalRule
import engine.asp.{Expiration, Outdate, TicksUntilExpiration, TicksUntilOutdated}

/**
  * Created by hb on 22.04.17.
  */
trait AnnotatedNormalRule {
  val rule: NormalRule
}

case class StaticNormalRule(rule: NormalRule) extends AnnotatedNormalRule

//rule that has to be deleted after ticksUntilExpired (resp. its groundings)
case class ExpiringNormalRuleTemplate(rule: NormalRule, ticksUntilExpired: TicksUntilExpiration) extends AnnotatedNormalRule //non-ground
case class ExpiringNormalRule(rule: NormalRule, expiration: Expiration) extends AnnotatedNormalRule //ground

//rule that has can be deleted after ticksUntilOutdated (resp. its groundings)
case class OutdatingNormalRuleTemplate(rule: NormalRule, ticksUntilOutdated: TicksUntilOutdated) extends AnnotatedNormalRule //non-ground
case class OutdatingNormalRule(rule: NormalRule, outdate: Outdate) extends AnnotatedNormalRule //ground

