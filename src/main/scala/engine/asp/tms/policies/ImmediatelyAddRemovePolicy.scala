package engine.asp.tms.policies

import core.asp._
import core.lars.TimePoint
import engine.Result
import engine.asp.GroundAspRule
import engine.asp.tms.GroundRule
import jtms.{TruthMaintenanceNetwork$, JtmsUpdateAlgorithm}

/**
  * Created by FM on 12.06.16.
  */
//TODO hb: maybe better call "ImmediateAddRemovePolicy"
case class ImmediatelyAddRemovePolicy(tms: JtmsUpdateAlgorithm = JtmsUpdateAlgorithm()) extends TmsPolicy {

  override def initialize(rules: Seq[NormalRule]) = rules foreach (tms.add(_))

  override def add(timePoint: TimePoint)(rules: Seq[NormalRule]): Unit = rules foreach (tms.add(_))

  override def remove(timePoint: TimePoint)(rules: Seq[NormalRule]): Unit = rules foreach (tms.remove(_))

  override def getModel(timePoint: TimePoint): Result = Result(tms.getModel())
}
