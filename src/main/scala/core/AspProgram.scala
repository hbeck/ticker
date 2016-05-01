package core

/**
  * Created by FM on 25.02.16.
  */
case class AspProgram(rules: List[Rule]) {
  val atoms = rules.flatMap(_.atoms)

  def +(rule: Rule) = AspProgram(rules :+ rule)

  def ++(rules: List[Rule]) = AspProgram(this.rules ++ rules)
}

object AspProgram {
  def apply(rules: Rule*): AspProgram = AspProgram(rules.toList)
}