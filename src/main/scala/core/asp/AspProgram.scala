package core.asp

import core.{Atom, AtomWithArgument, PinnedAtom}
import engine.asp.PinnedRule


/**
  * Created by FM on 25.02.16.
  */
trait AspProgram[TAtom <: Atom, TAspRule <: AspRule[TAtom]] {
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

case class ModifiableAspProgram(rules: List[NormalRule]) extends NormalProgram {

  def +(rule: NormalRule) = AspProgram(rules :+ rule)

  def ++(rules: List[NormalRule]) = AspProgram(this.rules ++ rules)
}

case class FixedAspProgram[TAtom <: Atom, TAspRule <: AspRule[TAtom]](rules: Seq[TAspRule]) extends AspProgram[TAtom, TAspRule]

object AspProgram {
  def apply(rules: NormalRule*): ModifiableAspProgram = ModifiableAspProgram(rules.toList)

  def apply(rules: List[NormalRule]): ModifiableAspProgram = ModifiableAspProgram(rules)

  // TODO: this should be generic?
  //  def apply[TAspRule <: AspRuleT[_]](rules: TAspRule*) = FixedAspProgram(rules.toList)
  def pinned(rules: PinnedRule*): FixedAspProgram[AtomWithArgument, PinnedRule]= FixedAspProgram[AtomWithArgument, PinnedRule](rules.toList)
  def pinned(rules: List[PinnedRule]): FixedAspProgram[AtomWithArgument, PinnedRule] = FixedAspProgram[AtomWithArgument, PinnedRule](rules)
}
