package engine.asp.tms.policies

import core.asp._
import core.lars.TimePoint
import engine.Result
import engine.asp.GroundAspRule
import engine.asp.tms.GroundRule
import jtms.{JtmsStorage, JtmsUpdateAlgorithm}

/**
  * Created by FM on 12.06.16.
  */
case class ImmediatelyAddRemovePolicy(tms: JtmsUpdateAlgorithm = JtmsUpdateAlgorithm()) extends TmsPolicy {

  override def initialize(groundRules: Seq[NormalRule]) = groundRules foreach (x => tms.add(x))

  override def add(timePoint: TimePoint)(rules: Seq[NormalRule]): Unit = rules foreach (x => tms.add(x))

  override def remove(timePoint: TimePoint)(rules: Seq[NormalRule]): Unit = rules foreach (x => tms.remove(x))

  override def getModel(timePoint: TimePoint): Result = Result(tms.getModel())
}
