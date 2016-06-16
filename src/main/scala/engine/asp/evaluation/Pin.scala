package engine.asp.evaluation

import core.asp._
import core.lars.{T, TimePoint, TimeVariableWithOffset}
import core.{Atom, PinnedAtom}

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
case class Pin(timePoint: TimePoint, variable: TimeVariableWithOffset = T) {

  def apply(atom: PinnedAtom): PinnedAtom = {
    val groundedBaseAtom = atom.timedAtom match {
      case t: PinnedAtom => apply(t)
      case a: Atom => a
    }

    // TODO: move into AtomWithTime.ground Function?
    val timeVariable = variable.variable

    val groundedTimePoint = atom.time match {
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
      //      if (g.time == timePoint)
      //        return g.atom

      GroundAtom(g)
    }
    case a: Atom => a
  }

  def ground(fact: NormalFact): GroundedNormalFact = GroundedNormalFact(this.ground(fact.head))

  def ground(rule: NormalRule): GroundedNormalRule = {
    GroundedNormalRule(
      this.ground(rule.head),
      rule.pos map this.ground,
      rule.neg map this.ground
    )
  }

  def ground(dataStream: PinnedStream): GroundedStream = apply(dataStream)

  def ground(rules: Seq[NormalRule]): Seq[GroundedNormalRule] = rules map ground

  def ground(program: NormalProgram, dataStream: PinnedStream): GroundedNormalProgram = {
    GroundedNormalProgram(
      program.rules map ground,
      ground(dataStream),
      timePoint
    )
  }

  def apply(dataStream: PinnedStream): Set[GroundedNormalFact] = dataStream map apply

  def apply(program: PinnedProgram, dataStream: PinnedStream): GroundedNormalProgram = {
    GroundedNormalProgram(
      program.rules map apply,
      apply(dataStream),
      timePoint
    )
  }

  def apply(pinnedFact: PinnedFact): GroundedNormalFact = GroundedNormalFact(this.apply(pinnedFact.head))

  def apply(pinnedAspRule: PinnedRule): GroundedNormalRule = {
    GroundedNormalRule(
      this.apply(pinnedAspRule.head),
      pinnedAspRule.pos map this.apply,
      pinnedAspRule.neg map this.apply
    )
  }
}

case class GroundedNormalRule(head: Atom, pos: Set[Atom] = Set(), neg: Set[Atom] = Set()) extends NormalRule

object GroundedNormalRule {

  def apply(rule: NormalRule): GroundedNormalRule = {
    if (rule.isGround) {
      GroundedNormalRule(rule.head, rule.pos, rule.neg)
    } else {
      throw new IllegalArgumentException("Cannot convert rule " + rule + " into a grounded Rule")
    }

  }
}

case class GroundedNormalProgram(programRules: Seq[GroundedNormalRule], groundedAtoms: GroundedStream, timePoint: TimePoint) extends NormalProgram {
  val rules: Seq[NormalRule] = programRules ++ groundedAtoms
}
