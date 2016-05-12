package engine

import core.lars.TimePoint
import core.{Atom, Model}


/**
  * Created by FM on 05.04.16.
  */
trait EvaluationEngine {

  def append(time: TimePoint)(atoms: Atom*): Unit

  def evaluate(time: TimePoint): Result
}

trait Result {
  def get: Option[Model]
}
