package engine.asp.tms.policies

import core.lars.TimePoint
import engine.Result
import engine.asp.GroundAspRule

/**
  * Created by FM on 12.06.16.
  */
trait TmsPolicy {
  def initialize(groundRules: Seq[GroundAspRule])

  def add(timePoint: TimePoint)(rules: Seq[GroundAspRule])

  def getModel(timePoint: TimePoint): Result

  def remove(timePoint: TimePoint)(rules: Seq[GroundAspRule])
}
