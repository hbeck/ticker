package core.lars

/**
  * Created by FM on 01.05.16.
  */
// TODO: Seq[Rule] (for evaluation purposes it seems more useful to have the order explicitly)
case class Program(rules: Set[Rule])

object Program {
  def apply(rules: Rule*): Program = Program(rules.toSet)
}