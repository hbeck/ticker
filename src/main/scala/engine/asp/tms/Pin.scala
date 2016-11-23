package engine.asp.tms

import core.asp._
import core.lars.{T, TimePoint, TimeVariableWithOffset}
import core._
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
case class Pin(timePoint: TimePoint, timeVariableWithOffset: TimeVariableWithOffset = T) {

  def apply(atom: Atom): PinnedFact = {
    AspFact(atom(timePoint))
  }

  def apply(aa: AtomWithArgument): AtomWithArgument = aa match {
    case pa: PinnedAtom => apply(pa)
    case _ => aa
  }

  def apply(pinnedAtom: PinnedAtom): PinnedAtom = {

    val groundedBaseAtom = pinnedAtom.atom match {
      case pa: PinnedAtom => apply(pa)
      case a: Atom => a
    }

    val timeVariable = timeVariableWithOffset.variable

    val groundedTimePoint = pinnedAtom.time match {
      case v@TimeVariableWithOffset(`timeVariable`, _) => v.ground(timePoint)
      // TODO: how should we ground an unknown time-variable? (e.g. w_1_a_U_a(U,T) :- now(T), a(U), reach(U,T).)
      case v: TimeVariableWithOffset => v.ground(timePoint)
      case t: TimePoint => t
    }

    groundedBaseAtom(groundedTimePoint)
  }

  def ground(atom: Atom): Atom = atom match {
    case p: PinnedAtom => {
      val g = this.apply(p)
//      ground(g)
      g
    }
    case a: GroundAtom => a
    case _ => throw new RuntimeException("cannot ground " + atom)
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
