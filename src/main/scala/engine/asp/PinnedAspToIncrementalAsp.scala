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

    AspRule(unpin(rule.head), rule.pos.filterNot(_.atom == now).map(unpinIfNeeded), rule.neg.map(unpinIfNeeded))
  }


  //  def apply(rule: MappedRule, headAtoms: Set[ExtendedAtom]): Set[AspRule[Atom]] = {
  //    // TODO: better way to find window-atoms?
  //    val windowAtoms = rule._1.body.filter(x => headAtoms.contains(x)).map(PlainLarsToAsp.apply)
  //
  //    rule._2.map(this.apply(_, windowAtoms))
  //  }

  def apply(p: MappedProgram): NormalProgram = {
    val headAtoms = p.mappedRules.flatMap(_._2.map(_.head)).toSet[ExtendedAtom]

    val strippedRules = p.rules.map(x => this.apply(x, headAtoms))

    AspProgram(strippedRules.toList)
  }

  def findFixPoint(normalProgram: NormalProgram) = {
    val g0 = GroundPinned(0).groundIfNeeded(normalProgram, Set())

    val fixedParts = normalProgram.rules.intersect(g0.rules)

    (fixedParts, normalProgram.rules.diff(fixedParts))
  }
}
