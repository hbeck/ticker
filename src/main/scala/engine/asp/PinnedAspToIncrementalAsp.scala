package engine.asp

import core.{Atom, PinnedAtom}
import core.asp._
import core.lars.{ExtendedAtom, WindowAtom}
import engine.asp.evaluation.{GroundPinned, MappedRule, PinnedRule}

/**
  * Created by FM on 08.06.16.
  */
object PinnedAspToIncrementalAsp {
  def unpin(pinned: PinnedAtom) = pinned.atom

  def apply(rule: PinnedRule, atomsToUnpin: Set[ExtendedAtom]): AspRule[Atom] = {

    def unpinIfNeeded(pinned: PinnedAtom) = atomsToUnpin.contains(pinned) match {
      case true => unpin(pinned)
      case false => pinned
    }

    AspRule(
      unpin(rule.head),
      rule.pos filterNot (_.atom == now) map unpinIfNeeded,
      rule.neg map unpinIfNeeded
    )
  }

  def apply(p: MappedProgram): NormalProgram = {
    val headAtoms = p.mappedRules.flatMap(r => r._2 map (_.head)).toSet[ExtendedAtom]

    val strippedRules = p.rules map (r => apply(r, headAtoms))

    AspProgram(strippedRules.toList)
  }
}
