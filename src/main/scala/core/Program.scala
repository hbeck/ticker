package core


object Program {
  def apply(rules: Rule*): Program = Program.apply(rules.toList)
}

/**
  * Created by FM on 25.02.16.
  */
case class Program(rules: List[Rule]) {
  val atoms = rules.flatMap(_.atoms).toSet

  def +(rule: Rule) = Program(this.rules.:+(rule))

  def ++(other: Program) = Program(this.rules ++ other.rules)

}
