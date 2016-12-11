package engine.asp.tms

import core.asp.{AspProgram, _}
import core.lars._
import core.{Atom, AtomWithArgument, PinnedAtom, Variable}
import engine.asp.{PinnedProgramWithLars, PinnedRule, now}

/**
  * Created by FM on 08.06.16.
  *
  * Remove temporal information (the pinned part, so to speak) from intensional atoms.
  */
object PinnedAspToIncrementalAsp {

  def unpin(atom: AtomWithArgument): Atom = atom match {
    case p: PinnedAtom => unpin(p)
    case _ => atom
  }

  def unpin(pinned: PinnedAtom): Atom = pinned.atom match {
    case p: PinnedAtom => p
    case _ => pinned.arguments match {
      case pinned.timeAsArgument :: Nil => pinned.time match {
        case t: TimeVariableWithOffset if t.variable == T.variable => pinned.atom
        case p: TimePoint => pinned.atom
        case _ => pinned
      }
      case _ => Atom(pinned.predicate, pinned.arguments filter (_ != pinned.timeAsArgument))
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
        // at rules have a concrete now, we want to keep that
        (rule.pos collect { case p: PinnedAtom if p.atom == now && p.time.isInstanceOf[TimePoint] => p }),
      rule.neg map unpinIfNeeded
    )
  }

  def apply(p: PinnedProgramWithLars): NormalProgram = {

    val windowAtoms = p.windowAtoms map (_.atom)
    val atAtoms = p.atAtoms map (_.atom)

    val timestampedAtoms = (windowAtoms ++ atAtoms) collect {
      case aa: AtomWithArgument => (aa.predicate, aa.arguments)
      case a: Atom => (a.predicate, Seq())
    }

    // get pinned window atoms (that is matching predicates and arguments, except the last argument which is the Time-Variable)
    val pinnedTimestamptedAtoms = p.atoms filter (a => timestampedAtoms.contains((a.predicate, a.arguments.init)))
    val atomAtT: Set[AtomWithArgument] = pinnedTimestamptedAtoms collect {
      case pinned: PinnedAtom if pinned.time == core.lars.T => pinned
    }

    val atomsToKeepPinned = pinnedTimestamptedAtoms diff atomAtT

    val atomsToUnpin = (p.atoms diff atomsToKeepPinned).toSet[ExtendedAtom]

    val headAtoms = p.larsRulesAsPinnedRules.flatMap(r => r._2 map (_.head)).toSet[ExtendedAtom] //i.e., intensional atoms
    //    val atomsToUnpin = headAtoms

    val semiPinnedRules = p.rules map (r => apply(r, atomsToUnpin))

    AspProgram(semiPinnedRules.toList)
  }
}
