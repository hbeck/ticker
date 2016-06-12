package engine.asp.evaluation.policies

import core._
import core.lars.TimePoint
import engine.asp.evaluation.GroundedNormalRule

/**
  * Created by FM on 12.06.16.
  */
trait TmsPolicy {
  def initialize(groundRules: Seq[GroundedNormalRule])

  def add(timePoint: TimePoint)(rules: Seq[GroundedNormalRule])

  def getModel(timePoint: TimePoint): Option[Model]

  def remove(timePoint: TimePoint)(rules: Seq[GroundedNormalRule])
}
