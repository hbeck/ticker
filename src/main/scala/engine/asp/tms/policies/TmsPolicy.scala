package engine.asp.tms.policies

import core._
import core.lars.TimePoint
import engine.Result
import engine.asp.GroundRule

/**
  * Created by FM on 12.06.16.
  */
trait TmsPolicy {
  def initialize(groundRules: Seq[GroundRule])

  def add(timePoint: TimePoint)(rules: Seq[GroundRule])

  def getModel(timePoint: TimePoint): Result

  // TODO: change to (timePoint) params only - intelligent strategy which rules are removed is part of the policy
  def remove(timePoint: TimePoint)(rules: Seq[GroundRule])
}
