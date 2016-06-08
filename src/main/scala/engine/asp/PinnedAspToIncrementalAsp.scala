package engine.asp

import core.{Atom, PinnedAtom}
import core.asp._
import core.lars.WindowAtom
import engine.asp.evaluation.{MappedRule, PinnedRule}

/**
  * Created by FM on 08.06.16.
  */
object PinnedAspToIncrementalAsp {
  def unpin(pinned: PinnedAtom) = pinned.atom

  def apply(rule: PinnedRule, windowAtoms: Set[PinnedAtom]): AspRule[Atom] = {

    def unpinIfNeeded(pinned: PinnedAtom) = windowAtoms.contains(pinned) match {
      case true => unpin(pinned)
      case false => pinned
    }

    AspRule(unpin(rule.head), rule.pos.filterNot(_.atom == now).map(unpinIfNeeded), rule.neg.map(unpinIfNeeded))
  }


  def apply(rule: MappedRule): Set[AspRule[Atom]] = {
    // TODO: better way to find window-atoms?
    val windowAtoms = rule._1.body.filter(_.isInstanceOf[WindowAtom]).map(PlainLarsToAsp.apply)

    rule._2.map(this.apply(_, windowAtoms))
  }

  def apply(p: MappedProgram): NormalProgram = {
    val strippedRules = p.mappedRules.flatMap(this.apply)
    AspProgram(strippedRules.toList)
  }
}
