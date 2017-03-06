package engine.asp.tms

import core.asp.{AspProgram, _}
import core.lars._
import core._
import engine.asp._

/**
  * Created by FM on 08.06.16.
  *
  * methods to remove tick-based information from mapped program (now, cnt stuff)
  */
object TickBasedAspToIncrementalAsp {

  def unpin(atom: AtomWithArgument): Atom = atom match {
    case p: PinnedAtAtom => unpin(p)
    case _ => atom
  }

  def unpin(pinned: PinnedTimeAtom): Atom = pinned.atom match {
    case p: PinnedAtom => p
    case _ => pinned.arguments match {
      case pinned.time :: Nil => pinned.time match {
        case t: TimeVariableWithOffset if t.variable == TimePinVariable.variable => pinned.atom
        case p: TimePoint => pinned.atom
        case _ => pinned
      }
      case _ => Atom(pinned.predicate, pinned.arguments filter (_ != pinned.time))
    }
  }

  def apply(rule: PinnedRule, atomsToUnpin: Set[ExtendedAtom]): AspRule[Atom] = {

    def unpinIfNeeded(pinned: AtomWithArgument) = atomsToUnpin.contains(pinned) match {
      case true => unpin(pinned)
      case false => pinned
    }

    AspRule(
      unpin(rule.head),
      (rule.pos filterNot (_.atom == now) map unpinIfNeeded) ++
        // @ with a concrete Timepoint (e.g. @_10 a)requires a now(t) => therefore we need to keep now when t is a concrete Timepoint
        (rule.pos collect { case p: PinnedTimeAtom if p.atom == now && p.time.isInstanceOf[TimePoint] => p }),
      rule.neg map unpinIfNeeded
    )
  }

  def apply(larsProgramEncoding: LarsProgramEncoding): NormalProgram = {
    val rulesWithoutNowCntPin = larsProgramEncoding.rules map stripTickAtoms
    AspProgram(rulesWithoutNowCntPin.toList)
  }

  def stripTickAtoms(rule: NormalRule): NormalRule = {
    rule.
      from(
        rule.head,
        rule.pos filterNot (a => engine.asp.specialTickPredicates.contains(a.predicate)),
        rule.neg
      ).
      asInstanceOf[NormalRule]
  }
}
