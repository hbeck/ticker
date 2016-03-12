package core

/**
  * Created by FM on 25.02.16.
  */
case class Program(rules: List[Rule]) {
  val atoms = rules.flatMap(_.atoms)
  def +(rule: Rule) = Program(rules :+ rule)
}

object Program {
  def apply(rules: Rule*): Program = Program(rules.toList)
}