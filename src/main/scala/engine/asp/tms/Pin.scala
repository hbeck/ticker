package engine.asp.tms

import core._
import core.asp._
import core.lars.{Assignment, TimePoint}
import engine.asp._

/**
  * Created by FM on 16.05.16.
  * Grounding a Program:
  * - a rule r = h <- e_1, ... e_n, not e_{n+1}, ..., not e_m
  * - atoms e_i = atom (x, v_1, ... v_n) with time-variables v_i
  * - given a time-point t  , and a time-variable v = 'T'
  *
  * groundRule(r, t, v) = {
  * r' = g(h) <- g(e_1), ..., g(e_n), not g(e_{n+1), ..., not g(e_m)
  *
  * g(a) = {
  * b = base(a)
  *
  * case b(x, v) =>  b'(x, t)
  * case b(x, v_i) => b'(x, v_i)
  * }
  * base(a) = {
  * case a(x, v_1, ..., v_n) => g(a(x, v_1, ... v_{n-1}))
  * case a(x, v) => a'(x)
  * }
  * }
  *
  * Discuss: how are we grounding other time-variables (unequal to T)?
  * e.g. w_1_a_U_a(U,T) :- now(T), a(U), reach(U,T).
  *
  */
case class Pin(assignment: Assignment) {

  def apply(aa: AtomWithArgument):Atom = {
    aa.assign(assignment).asInstanceOf[Atom]
  }

 /*
  //TODO why do we have to make a case distinction instead of uniformly calling assign on the AtomWithArgument?
  def apply(aa: AtomWithArgument): AtomWithArgument = aa match {
    case tca: PinnedTimeCntAtom => apply(tca)
    case pa: PinnedTimeAtom => apply(pa)
    case ca: PinnedCntAtom => apply(ca)
    case ng: NonGroundAtomWithArguments => apply(ng)
    case _ => aa
  }

  def apply(atom: NonGroundAtomWithArguments) = {
    atom.assign(assignment)
  }

  def apply(pinnedAtom: PinnedTimeAtom): PinnedAtom = {

    val groundedBaseAtom = pinnedAtom.atom match {
      case pa: PinnedAtom => apply(pa) //TODO
      case a: Atom => a
    }

    val timeVariables = assignment.binding collect {
      case (t: TimeVariableWithOffset, time: TimePoint) => (t.variable, time)
    }

    val groundedTimePoint = pinnedAtom.time match {
      case v: TimeVariableWithOffset => {
        // TODO: how should we ground an unknown time-variable? (e.g. w_1_a_U_a(U,T) :- now(T), a(U), reach(U,T).) ...
        if (timeVariables.contains(v.variable)) v.calculate(timeVariables(v.variable))
        else v  //... probably later, i.e., in the actual grounding. this is pinning.
      }
      case t: TimePoint => t
    }

    PinnedAtom(groundedBaseAtom, groundedTimePoint)
  }

  def apply(pinnedAtom: PinnedTimeCntAtom): PinnedAtom = {

    val timeVariables = assignment.binding.collect {
      case (t: TimeVariableWithOffset, time: TimePoint) => (t.variable, time)
    }

    val groundedTimePoint = pinnedAtom.time match {
      case v: TimeVariableWithOffset if timeVariables.contains(v.variable) => v.calculate(timeVariables(v.variable))
      // TODO: how should we ground an unknown time-variable? (e.g. w_1_a_U_a(U,T) :- now(T), a(U), reach(U,T).) ...
      case v: TimeVariableWithOffset => v //... probably later, i.e., in the actual grounding. this is pinning.
      case t: TimePoint => t
    }


    val countVariables = assignment.binding.collect {
      case (variable: Variable, value: Value) => (variable.name, value)
    }

    val groundedCount = pinnedAtom.cnt match {
      case v: Variable if countVariables.contains(v.name) => countVariables(v.name)
      case v: Variable => v //... probably later, i.e., in the actual grounding. this is pinning.
      case v: Value => v
    }

    PinnedAtom(pinnedAtom.atom, groundedTimePoint, groundedCount)
  }

  def apply(pinnedAtom: PinnedCntAtom): PinnedAtom = {

    val groundedBaseAtom = pinnedAtom.atom match {
      case pa: PinnedAtom => apply(pa) //TODO hb: infinite circle? if all methods are called apply, it is difficult to understand what's happening
      case a: Atom => a
    }

    val countVariables = assignment.binding.collect {
      case (variable: Variable, value: Value) => (variable.name, value)
    }

    val groundedCount = pinnedAtom.cnt match {
      case v: VariableWithOffset if countVariables.contains(v.name) => v.calculate(countVariables(v.name))
      case v: Variable if countVariables.contains(v.name) => countVariables(v.name)
      case v: Variable => v //... probably later, i.e., in the actual grounding. this is pinning.
      case v: Value => v
    }

    PinnedAtom.asCount(groundedBaseAtom, groundedCount)
  }
  */

  def ground(atom: Atom): Atom = atom match {
    case p: PinnedAtom => {
      val g = this.apply(p)
      //      ground(g) //TODO hb: why is this commented?
      g
    }
    case ng: NonGroundAtomWithArguments => ng.assign(assignment)
    case rel: RelationAtom if !rel.isGround() => rel.assign(assignment).asInstanceOf[Atom]
    case a: GroundAtom => a
    case _ => atom
    //    case _ => throw new RuntimeException("cannot ground " + atom)
  }

  def ground(fact: NormalFact): NormalFact = AspFact(this.ground(fact.head))

  def ground(rule: NormalRule): NormalRule = {
    AspRule(
      this.ground(rule.head),
      rule.pos map this.ground,
      rule.neg map this.ground
    )
  }

  //  def ground(pinnedAtom: PinnedAtom): GroundAtom = {
  //    GroundAtom(pinnedAtom.atom.predicate, pinnedAtom.arguments.map(_.asInstanceOf[Value]).toList: _*)
  //  }

  def ground(dataStream: PinnedStream): Set[NormalFact] = apply(dataStream)

  def ground(rules: Seq[NormalRule]): Seq[NormalRule] = rules map ground

  def apply(dataStream: PinnedStream): Set[NormalFact] = dataStream map apply

  def apply(pinnedFact: PinnedFact): NormalFact = AspFact(ground(this.apply(pinnedFact.head)))

  def apply(pinnedAspRule: PinnedRule): NormalRule = {
    AspRule(
      ground(this.apply(pinnedAspRule.head)),
      pinnedAspRule.pos map this.apply map this.ground,
      pinnedAspRule.neg map this.apply map this.ground
    )
  }
}

object Pin {

  def apply(time: TimePoint):Pin = {
    Pin(
      Assignment(
        Map(
          core.lars.TimePinVariable -> time
        )
      )
    )
  }

  def apply(count: Long):Pin = {
    Pin(
      Assignment(
        Map(
          core.lars.CountPinVariable -> IntValue(count.toInt)
        )
      )
    )
  }

  def apply(time: TimePoint, count: Long):Pin = {
    Pin(
      Assignment(
        Map(
          core.lars.TimePinVariable -> time,
          core.lars.CountPinVariable -> IntValue(count.toInt)
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
