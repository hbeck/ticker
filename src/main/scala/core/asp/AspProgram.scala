package core.asp

/**
  * Created by FM on 25.02.16.
  */
trait AspProgram {
  val rules: Seq[AspRule]
  val atoms = () => this.rules.flatMap(_.atoms)
}

case class ModifiableAspProgram(rules: List[AspRule]) extends AspProgram {

  def +(rule: AspRule) = AspProgram(rules :+ rule)

  def ++(rules: List[AspRule]) = AspProgram(this.rules ++ rules)
}

object AspProgram {
  def apply(rules: AspRule*): ModifiableAspProgram = ModifiableAspProgram(rules.toList)

  def apply(rules: List[AspRule]): ModifiableAspProgram = ModifiableAspProgram(rules)
}