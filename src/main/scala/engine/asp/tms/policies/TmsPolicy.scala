package engine.asp.tms.policies

import core.asp.NormalRule
import core.lars.TimePoint
import engine.Result
import engine.asp.GroundAspRule

/**
  * Created by FM on 12.06.16.
  */
trait TmsPolicy {
  def initialize(groundRules: Seq[NormalRule])

  def add(timePoint: TimePoint)(rules: Seq[NormalRule])

  def getModel(timePoint: TimePoint): Result

  def remove(timePoint: TimePoint)(rules: Seq[NormalRule])
}
