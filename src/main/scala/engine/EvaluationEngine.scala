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

object EmptyResult extends Result {
  override def get: Option[Model] = Some(Set())
}

object NoResult extends Result {
  override def get: Option[Model] = None
}

object UnknownResult extends Result {
  override def get: Option[Model] = throw new IllegalStateException("No known model was derived")
}

object Result {
  def apply(model: Option[Model]): Result = model match {
    case None => NoResult
    case Some(m) => new Result {
      override def get: Option[Model] = Some(m)
    }
  }
}