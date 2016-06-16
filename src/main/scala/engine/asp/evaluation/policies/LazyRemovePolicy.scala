package engine.asp.evaluation.policies

import core._
import core.lars.{Duration, TimePoint}
import engine.asp.evaluation.GroundedNormalRule
import jtms.{Jtms, JtmsExtended}

import scala.collection.mutable

/**
  * Created by FM on 12.06.16.
  */
case class LazyRemovePolicy(tms: Jtms = JtmsExtended(), laziness: Duration = 0) extends TmsPolicy {

  // TODO: Set or Seq? guess Set because guarantee of order might be hard
  var markedForDelete: mutable.Map[TimePoint, Set[GroundedNormalRule]] = mutable.Map()
  var reverseDeleteMap: mutable.Map[GroundedNormalRule, TimePoint] = mutable.Map()

  def containsRule(r: GroundedNormalRule) = markedForDelete.values.exists(_ == r)

  override def initialize(groundRules: Seq[GroundedNormalRule]) = groundRules foreach tms.add

  override def remove(timePoint: TimePoint)(rules: Seq[GroundedNormalRule]): Unit = {
    rules foreach markAsDeleted(timePoint)
  }

  override def getModel(timePoint: TimePoint): Option[Model] = tms.getModel()

  override def add(timePoint: TimePoint)(rules: Seq[GroundedNormalRule]): Unit = {
    val markedAsDeleteEntries = reverseDeleteMap filter (x => rules.contains(x._1))
    // We don't need to add these rules - instead don't remove them
    markedAsDeleteEntries foreach (x => unmarkAsDeleted(x._1, x._2))

    val newRules = rules filterNot markedAsDeleteEntries.contains

    newRules foreach tms.add

    removeExpiredRules(timePoint)
  }

  def markAsDeleted(timePoint: TimePoint)(rule: GroundedNormalRule) = {
    reverseDeleteMap(rule) = timePoint

    val r = markedForDelete.getOrElse(timePoint, Set()) + rule

    markedForDelete.update(timePoint, r)
  }

  def unmarkAsDeleted(rule: GroundedNormalRule, timePoint: TimePoint) = {
    reverseDeleteMap.remove(rule)

    val notDeleted = markedForDelete.getOrElse(timePoint, Set()) - rule
    if (notDeleted.isEmpty)
      markedForDelete.remove(timePoint)
    else
      markedForDelete(timePoint) = notDeleted
  }

  def removeExpiredRules(timePoint: TimePoint): Unit = {
    val expiredTimePoints = markedForDelete.keys filter (_.value < timePoint.value - laziness)
    expiredTimePoints foreach (t => {
      val rules = markedForDelete.remove(t).get
      rules foreach (r => {
        tms.remove(r)
        reverseDeleteMap remove (r)
      })
    })
  }
}
