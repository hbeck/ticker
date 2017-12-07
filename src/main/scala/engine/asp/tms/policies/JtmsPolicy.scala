package engine.asp.tms.policies

import core.asp.NormalRule
import core.lars.TimePoint
import engine.Result

/**
  * Created by FM on 12.06.16.
  */
trait JtmsPolicy {
  def initialize(rules: Seq[NormalRule])

  def add(timePoint: TimePoint)(rules: Seq[NormalRule])

  def getModel(timePoint: TimePoint): Result

  def remove(timePoint: TimePoint)(rules: Seq[NormalRule])
}
