package core


object Program {
  def apply(rules: Rule*): Program = Program.apply(rules.toSet)
}

/**
  * Created by FM on 25.02.16.
  */
case class Program(rules: Set[Rule]) {
  val atoms = rules.flatMap(_.atoms)
}
