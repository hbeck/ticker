package core.asp

import core.Atom


/**
  * Created by FM on 25.02.16.
  */
trait AspProgram[TAtom <: Atom, TAspRule <: AspRule[TAtom]] {
  val rules: Seq[TAspRule]
  lazy val atoms = this.rules.flatMap(_.atoms).toSet
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

case class AppendableAspProgram(rules: List[NormalRule]) extends NormalProgram {

  def +(rule: NormalRule) = AppendableAspProgram(rules :+ rule)

  def ++(rules: List[NormalRule]) = AppendableAspProgram(this.rules ++ rules)
}

////TODO hb what is this?
//case class FixedAspProgram[TAtom <: Atom, TAspRule <: AspRule[TAtom]](rules: Seq[TAspRule]) extends AspProgram[TAtom, TAspRule]

object AspProgram {
  def apply(rules: NormalRule*): AppendableAspProgram = AppendableAspProgram(rules.toList)

  def apply(rules: List[NormalRule]): AppendableAspProgram = AppendableAspProgram(rules)

  //def pinned(rules: PinnedRule*): FixedAspProgram[AtomWithArgument, PinnedRule]= FixedAspProgram[AtomWithArgument, PinnedRule](rules.toList)

  //def pinned(rules: List[PinnedRule]): FixedAspProgram[AtomWithArgument, PinnedRule] = FixedAspProgram[AtomWithArgument, PinnedRule](rules)
}
