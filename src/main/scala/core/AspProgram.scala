package core

/**
  * Created by FM on 25.02.16.
  */
case class AspProgram(rules: List[AspRule]) {
  val atoms = rules.flatMap(_.atoms)

  def +(rule: AspRule) = AspProgram(rules :+ rule)

  def ++(rules: List[AspRule]) = AspProgram(this.rules ++ rules)
}

object AspProgram {
  def apply(rules: AspRule*): AspProgram = AspProgram(rules.toList)
}