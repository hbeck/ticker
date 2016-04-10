package engine


/**
  * Created by FM on 05.04.16.
  */
trait EvaluationEngine {

  def append(time: Time)(atoms: Atom*): Unit

  def evaluate(time: Time): Set[Atom]
}
