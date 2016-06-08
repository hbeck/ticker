package engine.asp.evaluation

import core.asp.{AspRule, NormalProgram, NormalRule}
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
case class GroundPinned(timePoint: TimePoint, variable: TimeVariableWithOffset = T) {
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

  def groundIfNeeded(atom: Atom): Atom = atom match {
    case p: PinnedAtom => {
      val g = this.apply(p)
      if (g.time == timePoint)
        return g.atom

      return g
    }
    case a: Atom => a
  }

  def groundIfNeeded(rule: NormalRule): GroundedNormalRule = {
    GroundedNormalRule(
      this.groundIfNeeded(rule.head),
      rule.pos map this.groundIfNeeded,
      rule.neg map this.groundIfNeeded
    )
  }

  def groundIfNeeded(program: NormalProgram, dataStream: PinnedStream): GroundedNormalProgram = {
    val atoms = dataStream map  apply map groundIfNeeded

    GroundedNormalProgram(program.rules map groundIfNeeded, atoms, timePoint)
  }

  def apply(program: PinnedProgram, dataStream: PinnedStream): GroundedNormalProgram = {
    val atoms = dataStream map apply

    GroundedNormalProgram(program.rules map apply, atoms, timePoint)
  }

  def apply(pinnedAspRule: PinnedRule): GroundedNormalRule = {
    GroundedNormalRule(
      this.apply(pinnedAspRule.head),
      pinnedAspRule.pos map this.apply,
      pinnedAspRule.neg map this.apply
    )
  }
}

case class GroundedNormalRule(head: Atom, pos: Set[Atom] = Set(), neg: Set[Atom] = Set()) extends AspRule[Atom]

//case class GroundedAspFact(head: Atom) extends AspRule[Atom]{
//  val pos = Set()
//  val neg = Set()
//}

case class GroundedNormalProgram(programRules: Seq[GroundedNormalRule], groundedAtoms: GroundedStream, timePoint: TimePoint) extends NormalProgram {
  val rules: Seq[NormalRule] = programRules ++ groundedAtoms
}
