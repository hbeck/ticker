package reasoner.asp.tms

import core._
import core.asp._
import core.lars.{Assignment, TimePoint}
import reasoner.asp._
import reasoner.asp.CountPinVariable

/**
  * Created by FM on 16.05.16.
  *
  */
case class Pin(assignment: Assignment) {

  def apply(aa: AtomWithArguments):Atom = {
    aa.assign(assignment)
  }

  def groundTickVariables(atom: Atom): Atom = atom match {
    case rel: RelationAtom if !rel.isGround() => rel.assign(assignment).asInstanceOf[RelationAtom]
    case p: PinnedAtom => this.apply(p)
    case ng: NonGroundAtomWithArguments => ng.assign(assignment)
    case a: GroundAtom => a
    case _ => atom
  }

  def groundTickVariables(fact: NormalFact): NormalFact = AspFact(this.groundTickVariables(fact.head))

  def groundTickVariables(rule: NormalRule): NormalRule = {
    AspRule(
      this.groundTickVariables(rule.head),
      rule.pos map this.groundTickVariables,
      rule.neg map this.groundTickVariables
    )
  }

  def groundTickVariables(dataStream: PinnedStream): Set[NormalFact] = apply(dataStream)

  def groundTickVariables(rules: Seq[NormalRule]): Seq[NormalRule] = rules map groundTickVariables

  def apply(dataStream: PinnedStream): Set[NormalFact] = dataStream map apply

  def apply(pinnedFact: PinnedFact): NormalFact = AspFact(groundTickVariables(this.apply(pinnedFact.head)))

  def apply(pinnedAspRule: PinnedRule): NormalRule = {
    AspRule(
      groundTickVariables(this.apply(pinnedAspRule.head)),
      pinnedAspRule.pos map this.apply map this.groundTickVariables,
      pinnedAspRule.neg map this.apply map this.groundTickVariables
    )
  }
}

object Pin {

  def apply(time: TimePoint):Pin = {
    Pin(
      Assignment(
        Map(
          TimePinVariable -> time
        )
      )
    )
  }

  def apply(count: Long):Pin = {
    Pin(
      Assignment(
        Map(
          CountPinVariable -> IntValue(count.toInt)
        )
      )
    )
  }

  def apply(time: TimePoint, count: Long):Pin = {
    Pin(
      Assignment(
        Map(
          TimePinVariable -> time,
          CountPinVariable -> IntValue(count.toInt)
        )
      )
    )
  }
}

object GroundedNormalRule {

  def apply(rule: NormalRule): GroundAspRule = {
    if (rule.isGround) {
      AspRule(
        rule.head.asInstanceOf[GroundAtom],
        rule.pos map (_.asInstanceOf[GroundAtom]),
        rule.neg map (_.asInstanceOf[GroundAtom])
      )
    } else {
      throw new IllegalArgumentException("Cannot convert rule " + rule + " into a grounded Rule")
    }
  }

}

object GroundRule {
  def asNormalRule(rule: GroundAspRule): NormalRule = {
    AspRule(rule.head.asInstanceOf[Atom], rule.pos map (_.asInstanceOf[Atom]), rule.neg map (_.asInstanceOf[Atom]))
  }
}
