package reasoner.asp.tms.policies

import core.asp._
import core.lars.TimePoint
import reasoner.Result
import reasoner.incremental.jtms.algorithms.Jtms
import reasoner.incremental.policies.JtmsPolicy

/**
  * Created by FM on 12.06.16.
  */
case class ImmediatelyAddRemovePolicy(jtms: Jtms = Jtms()) extends JtmsPolicy {

  override def initialize(rules: Seq[NormalRule]) = rules foreach (jtms.add(_))

  override def add(timePoint: TimePoint)(rules: Seq[NormalRule]): Unit = rules foreach (jtms.add(_))

  override def remove(timePoint: TimePoint)(rules: Seq[NormalRule]): Unit = rules foreach (jtms.remove(_))

  override def getModel(timePoint: TimePoint): Result = Result(jtms.getModel())

}
