package reasoner.common

import core.asp.{NormalProgram, NormalRule}
import core.lars.{LarsBasedProgram, LarsRule}
import core.{Atom, Predicate}
import reasoner.{TickDuration,Void}
import reasoner.incremental.{AnnotatedNormalRule, IncrementalAspPreparation}


/**
  * Created by fm on 20/02/2017.
  *
  * to derive window atom encoding
  */
trait WindowAtomEncoder {

  val allWindowRules: Seq[NormalRule]

  val size: Long

  //non-instantiated incremental rules for (partial) pre-grounding
  def windowRuleTemplates(): Seq[AnnotatedNormalRule]

  val groundingGuards: Set[Atom]

}

trait TimeWindowEncoder extends WindowAtomEncoder

trait TupleWindowEncoder extends WindowAtomEncoder

/*
   E.g.   c(X) :- win2 \Diamond a(X), not win3 \Box b(X)  //larsRule
   ==>    c(X) :- w_2_d_a(X), w_3_b_b(X)   //ruleEncoding
          atoms w_2_d_a(X)  and  w_3_b_b(X) are called windowAtomEncodings and get their WindowAtomEncoder objects
 */
case class LarsRuleEncoding(larsRule: LarsRule, baseRule: NormalRule, posWindowAtomEncoders: Set[WindowAtomEncoder], negWindowAtomEncoders: Set[WindowAtomEncoder]) {

  lazy val windowAtomEncoders: Set[WindowAtomEncoder] = posWindowAtomEncoders ++ negWindowAtomEncoders
  /*
   * ticks that needed to be added to the respective pins to obtain the time/count, when the rule itself expires.
   * in contrast to window rules, we may keep them longer
   */
  val ticksUntilBaseRuleIsIrrelevant: TickDuration = {
    val maxTimeWindowSize = posWindowAtomEncoders.collect{ case e:TimeWindowEncoder => e.size}.foldLeft(-2L)((t1,t2) => Math.max(t1,t2))
    val maxTupleWindowSize = posWindowAtomEncoders.collect{ case e:TupleWindowEncoder => e.size}.foldLeft(-2L)((t1,t2) => Math.max(t1,t2))
    if (maxTimeWindowSize == -1 && maxTupleWindowSize == -1) { //0 might be a window length (at least for time windows)
      Tick(Void,Void)
    } else if (maxTupleWindowSize == -1) {
      Tick(maxTimeWindowSize + 1, Void)
    } else if (maxTimeWindowSize == -1) {
      Tick(Void,maxTupleWindowSize)
    } else {
      Tick(maxTimeWindowSize+1,maxTupleWindowSize)
    }
  }

}

case class LarsProgramEncoding(larsRuleEncodings: Seq[LarsRuleEncoding], nowAndAtNowIdentityRules: Seq[NormalRule], backgroundKnowledge: Seq[NormalRule], needGuard: Set[Predicate]) extends NormalProgram with LarsBasedProgram {

  lazy val windowAtomEncoders = larsRuleEncodings flatMap (_.windowAtomEncoders)

  /*
   * one-shot stuff
   */
  lazy val oneShotBaseRules = (larsRuleEncodings map (_.baseRule)) ++ nowAndAtNowIdentityRules ++ backgroundKnowledge

  lazy val (groundBaseRules, nonGroundBaseRules) = oneShotBaseRules.
    map(IncrementalAspPreparation.stripPositionAtoms).
    partition(_.isGround)

  lazy val oneShotWindowRules = windowAtomEncoders flatMap (_.allWindowRules)

  /*
   * general stuff
   */

  // full representation of Lars-Program as asp
  override lazy val rules = oneShotBaseRules ++ oneShotWindowRules

  override lazy val larsRules = larsRuleEncodings map (_.larsRule)

  //TODO hb 1802
  lazy val maximumTimeWindowSizeInTicks: Long = larsRuleEncodings.
    flatMap(_.windowAtomEncoders).
    collect {
      case t: TimeWindowEncoder => t.size
    } match {
    case Nil => 0
    case x => x.max
  }

}
