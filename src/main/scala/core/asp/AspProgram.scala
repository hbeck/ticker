package core.asp

import core.{Atom, Program}


/**
  * Created by FM on 25.02.16.
  */
trait AspProgram[TAtom <: Atom, TRule <: AspRule[TAtom]] extends Program[TRule,TAtom,TAtom] {
  val rules: Seq[TRule]
  lazy val atoms: Set[TAtom] = this.rules.flatMap(_.atoms).toSet
}

case class AppendableAspProgram(rules: List[NormalRule]) extends NormalProgram {

  def +(rule: NormalRule) = AppendableAspProgram(rules :+ rule)

  def ++(rules: List[NormalRule]) = AppendableAspProgram(this.rules ++ rules)
}

object AspProgram {
  def apply(rules: NormalRule*): AppendableAspProgram = AppendableAspProgram(rules.toList)

  def apply(rules: List[NormalRule]): AppendableAspProgram = AppendableAspProgram(rules)
}
