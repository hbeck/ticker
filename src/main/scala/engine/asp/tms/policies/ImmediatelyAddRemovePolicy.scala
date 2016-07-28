package engine.asp.tms.policies

import core._
import core.lars.TimePoint
import engine.Result
import engine.asp.GroundRule
import engine.asp.tms.GroundRule
import jtms.{Jtms, JtmsGreedy$}

/**
  * Created by FM on 12.06.16.
  */
case class ImmediatelyAddRemovePolicy(tms: Jtms = JtmsGreedy()) extends TmsPolicy {

  override def initialize(groundRules: Seq[GroundRule]) = groundRules foreach (x => tms.add(GroundRule.toNormalRule(x)))

  override def add(timePoint: TimePoint)(rules: Seq[GroundRule]): Unit = rules foreach (x => tms.add(GroundRule.toNormalRule(x)))

  override def remove(timePoint: TimePoint)(rules: Seq[GroundRule]): Unit = rules foreach (x => tms.remove(GroundRule.toNormalRule(x)))

  override def getModel(timePoint: TimePoint): Result = Result(tms.getModel())
}
