package engine.asp.evaluation.policies

import core._
import core.lars.TimePoint
import engine.asp.evaluation.GroundedNormalRule
import jtms.{Jtms, JtmsExtended}

/**
  * Created by FM on 12.06.16.
  */
case class DirectAddRemovePolicy(tms: Jtms = JtmsExtended()) extends TmsPolicy {

  override def initialize(groundRules: Seq[GroundedNormalRule]) = groundRules foreach tms.add

  override def add(timePoint: TimePoint)(rules: Seq[GroundedNormalRule]): Unit = rules foreach tms.add

  override def remove(timePoint: TimePoint)(rules: Seq[GroundedNormalRule]): Unit = rules foreach tms.remove

  override def getModel(timePoint: TimePoint): Option[Model] = tms.getModel()
}
