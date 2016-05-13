package core.asp

/**
  * Created by FM on 25.02.16.
  */
trait AspProgram {
  val rules: Seq[AspRule]
  val atoms = () => this.rules.flatMap(_.atoms)
}

case class AspProgram2(rules: List[AspRule]) extends AspProgram {

  def +(rule: AspRule) = AspProgram(rules :+ rule)

  def ++(rules: List[AspRule]) = AspProgram(this.rules ++ rules)
}

object AspProgram {
  def apply(rules: AspRule*): AspProgram2 = AspProgram2(rules.toList)

  def apply(rules: List[AspRule]): AspProgram2 = AspProgram2(rules)
}