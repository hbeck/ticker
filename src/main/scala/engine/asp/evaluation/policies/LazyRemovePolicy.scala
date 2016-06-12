package engine.asp.evaluation.policies

import core._
import core.lars.TimePoint
import engine.asp.evaluation.GroundedNormalRule
import jtms.{Jtms, JtmsExtended}

import scala.collection.mutable

/**
  * Created by FM on 12.06.16.
  */
case class LazyRemovePolicy(tms: Jtms = JtmsExtended()) extends TmsPolicy {

  var addedRules: Map[TimePoint, Seq[GroundedNormalRule]] = Map()
  var markedForDelete: mutable.Set[GroundedNormalRule] = mutable.Set()

  def containsRule(r: GroundedNormalRule) = addedRules.values.exists(_ == r)

  override def initialize(groundRules: Seq[GroundedNormalRule]) = groundRules foreach tms.add

  override def remove(timePoint: TimePoint)(rules: Seq[GroundedNormalRule]): Unit = {
    markedForDelete = markedForDelete union rules.toSet
  }

  override def getModel(timePoint: TimePoint): Option[Model] = tms.getModel()

  override def add(timePoint: TimePoint)(rules: Seq[GroundedNormalRule]): Unit = {
    val (rulesThatWouldBeReadded, unknownRules) = rules.partition(markedForDelete.contains(_))

    // We don't need to add these rules - instead don't remove them
    rulesThatWouldBeReadded foreach markedForDelete.remove

    unknownRules foreach {
      addedRules(timePoint)(_)
      tms.add(_)
    }

    markedForDelete foreach tms.remove
    markedForDelete.clear()
  }
}
