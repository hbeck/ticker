package engine.asp

import core.{Atom, Predicate}
import core.asp.{NormalProgram, NormalRule}
import core.lars.{LarsBasedProgram, LarsRule, TimePoint}
import engine.asp.tms.{AnnotatedNormalRule, TickBasedAspToIncrementalAsp}


/**
  * Created by fm on 20/02/2017.
  */
//to derive window atom encoding
trait WindowAtomEncoder {

  val allWindowRules: Seq[NormalRule]

  val length: Long
  //naming: *expiration* is a tick when a rule *must* be removed, whereas an *outdated* rule *can* be removed
  def ticksUntilWindowAtomIsOutdated(): TickDuration

  //non-instantiated incremental rules for (partial) pre-grounding
  def windowRuleTemplates(): Seq[AnnotatedNormalRule]

  val groundingGuards: Set[Atom] //TODO at this to posBody of windowRuleTemplates? 0424

}

trait TimeWindowEncoder extends WindowAtomEncoder

trait TupleWindowEncoder extends WindowAtomEncoder

/*
   E.g.   c(X) :- win2 \Diamond a(X), not win3 \Box b(X)  //larsRule
   ==>    c(X) :- w_2_d_a(X), w_3_b_b(X)   //ruleEncoding
          atoms w_2_d_a(X)  and  w_3_b_b(X) are called windowAtomEncodings and get their WindowAtomEncoder objects
 */
case class LarsRuleEncoding(larsRule: LarsRule, aspRule: NormalRule, windowAtomEncoders: Set[WindowAtomEncoder]) {
  /*
   * ticks that needed to be added to the respective pins to obtain the time/count, when the rule itself expires.
   * in contrast to window rules, we may keep them longer
   */
  def ticksUntilOutdated(): TickDuration = (windowAtomEncoders map (_.ticksUntilWindowAtomIsOutdated)).foldLeft(Tick(Void,Void))((ticks1, ticks2) => Tick.min(ticks1,ticks2))

}

case class LarsProgramEncoding(larsRuleEncodings: Seq[LarsRuleEncoding], nowAndAtNowIdentityRules: Seq[NormalRule], backgroundKnowledge: Seq[NormalRule], needGuard: Set[Predicate]) extends NormalProgram with LarsBasedProgram {

  lazy val windowAtomEncoders = larsRuleEncodings flatMap (_.windowAtomEncoders)
  //val baseRules = (larsRuleEncodings map (_.aspRule)) ++ (backgroundData map (AspFact(_))) //nowAndAtNowIdentityRules, which includes now(.)

  /*
   * one-shot stuff
   */
  lazy val oneShotBaseRules = (larsRuleEncodings map (_.aspRule)) ++ nowAndAtNowIdentityRules ++ backgroundKnowledge

  lazy val (groundBaseRules, nonGroundBaseRules) = oneShotBaseRules.
    map(TickBasedAspToIncrementalAsp.stripPositionAtoms).
    partition(_.isGround)

  lazy val oneShotWindowRules = windowAtomEncoders flatMap (_.allWindowRules)

  /*
   * general stuff
   */

  // full representation of Lars-Program as asp
  override lazy val rules = oneShotBaseRules ++ oneShotWindowRules

  override lazy val larsRules = larsRuleEncodings map (_.larsRule)

  lazy val maximumTimeWindowSizeInTicks: Long = larsRuleEncodings.
    flatMap(_.windowAtomEncoders).
    collect {
      case t: TimeWindowEncoder => t.length
    } match {
    case Nil => 0
    case x => x.max
  }

}

@deprecated
case class IncrementalRules(toAdd: Seq[NormalRule], toRemove: Seq[NormalRule]) {
  def ++(other: IncrementalRules) = IncrementalRules(toAdd ++ other.toAdd, toRemove ++ other.toRemove)
}

@deprecated
case class TickPosition(time: TimePoint, count: Long) //TODO remove this

