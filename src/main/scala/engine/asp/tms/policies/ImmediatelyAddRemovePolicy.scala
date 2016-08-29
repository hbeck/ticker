package engine.asp.tms.policies

import core.lars.TimePoint
import engine.Result
import engine.asp.GroundAspRule
import engine.asp.tms.GroundRule
import jtms.{Jtms, JtmsGreedy}

/**
  * Created by FM on 12.06.16.
  */
case class ImmediatelyAddRemovePolicy(tms: Jtms = JtmsGreedy()) extends TmsPolicy {

  override def initialize(groundRules: Seq[GroundAspRule]) = groundRules foreach (x => tms.add(GroundRule.asNormalRule(x)))

  override def add(timePoint: TimePoint)(rules: Seq[GroundAspRule]): Unit = rules foreach (x => tms.add(GroundRule.asNormalRule(x)))

  override def remove(timePoint: TimePoint)(rules: Seq[GroundAspRule]): Unit = rules foreach (x => tms.remove(GroundRule.asNormalRule(x)))

  override def getModel(timePoint: TimePoint): Result = Result(tms.getModel())
}
