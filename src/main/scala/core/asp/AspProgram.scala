package core.asp

import core.{Atom, PinnedAtom}
import engine.asp.evaluation.PinnedAspRule

/**
  * Created by FM on 25.02.16.
  */
trait AspProgramT[TAtom <: Atom, TAspRule <: AspRuleT[TAtom]] {
  val rules: Seq[TAspRule]
  lazy val atoms = this.rules.flatMap(_.atoms)
}

//trait AspProgramTT[TAtom <: Atom] {
//  type TAspRule <: AspRuleT[TAtom]
//  val rules: Seq[TAspRule]
//  lazy val atoms = this.rules.flatMap(_.atoms).toSet
//}
//
//trait AspProgramTB[TAspRule <: AspRuleT[TAtom]] {
//  type TAtom <: Atom
//  val rules: Seq[TAspRule]
//  lazy val atoms = this.rules.flatMap(_.atoms).toSet
//}

case class ModifiableAspProgram(rules: List[PlainAspRule]) extends PlainAspProgram {

  def +(rule: PlainAspRule) = AspProgram(rules :+ rule)

  def ++(rules: List[PlainAspRule]) = AspProgram(this.rules ++ rules)
}

case class FixedAspProgram[TAtom <: Atom, TAspRule <: AspRuleT[TAtom]](rules: Seq[TAspRule]) extends AspProgramT[TAtom, TAspRule]

object AspProgram {
  def apply(rules: PlainAspRule*): ModifiableAspProgram = ModifiableAspProgram(rules.toList)

  def apply(rules: List[PlainAspRule]): ModifiableAspProgram = ModifiableAspProgram(rules)

  // TODO: this should be generic?
  //  def apply[TAspRule <: AspRuleT[_]](rules: TAspRule*) = FixedAspProgram(rules.toList)
  def pinned(rules: PinnedAspRule*) :FixedAspProgram[PinnedAtom, PinnedAspRule]= FixedAspProgram[PinnedAtom, PinnedAspRule](rules.toList)
  def pinned(rules: List[PinnedAspRule]):FixedAspProgram[PinnedAtom, PinnedAspRule] = FixedAspProgram[PinnedAtom, PinnedAspRule](rules)
}
