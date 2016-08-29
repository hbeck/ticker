package engine.asp.tms

import core.asp.{AspProgram, _}
import core.lars.ExtendedAtom
import core.{Atom, AtomWithArgument, PinnedAtom}
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

  def unpin(pinned: PinnedAtom): Atom = pinned.arguments match {
    case pinned.timeAsArgument :: Nil => pinned.atom
    case _ => Atom(pinned.predicate, pinned.arguments filter (_ != pinned.timeAsArgument))
  }

  def apply(rule: PinnedRule, atomsToUnpin: Set[ExtendedAtom]): AspRule[Atom] = {

    def unpinIfNeeded(pinned: AtomWithArgument) = atomsToUnpin.contains(pinned) match {
      case true => unpin(pinned)
      case false => pinned
    }

    AspRule(
      unpin(rule.head),
      rule.pos filterNot (_.atom == now) map unpinIfNeeded,
      rule.neg map unpinIfNeeded
    )
  }

  def apply(p: PinnedProgramWithLars): NormalProgram = {
    val headAtoms = p.larsRulesAsPinnedRules.flatMap(r => r._2 map (_.head)).toSet[ExtendedAtom] //i.e., intensional atoms

    val semiPinnedRules = p.rules map (r => apply(r, headAtoms))

    AspProgram(semiPinnedRules.toList)
  }
}
