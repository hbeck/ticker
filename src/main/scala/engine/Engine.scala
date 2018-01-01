package engine

import core.lars.TimePoint
import core.{Atom, Model}


/**
  * Created by FM on 05.04.16.
  */
trait Evaluation {

  def append(timePoint: TimePoint)(atoms: Atom*): Unit

  def evaluate(timePoint: TimePoint): Result
}

trait Result {
  def get: Option[Model]

  def model: Model = get.get
}

object EmptyResult extends Result {
  override def get: Option[Model] = Some(Set())
}

object NoResult extends Result {
  override def get: Option[Model] = None
}

case class UnknownResult(info: String = "") extends Result {
  override def get: Option[Model] = throw new IllegalStateException("No model was derived. " + info)
}

object Result {

  private case class ResultModel(override val model: Model) extends Result {
    override def get: Option[Model] = Some(model)
  }

  def apply(m: Model): Result = {
    if (m.isEmpty)
      EmptyResult
    else
      ResultModel(m)
  }

  def apply(model: Option[Model]): Result = model match {
    case None => NoResult
    case Some(m) => Result(m)
  }
}