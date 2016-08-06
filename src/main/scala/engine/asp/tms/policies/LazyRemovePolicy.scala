package engine.asp.tms.policies

import core.lars.{Duration, TimePoint}
import engine.Result
import engine.asp.GroundRule
import engine.asp.tms.GroundRule
import jtms.{Jtms, JtmsGreedy}

import scala.collection.mutable

/**
  * Created by FM on 12.06.16.
  */
case class LazyRemovePolicy(tms: Jtms = JtmsGreedy(), laziness: Duration = 0) extends TmsPolicy {


  // TODO: Set or Seq? guess Set because guarantee of order might be hard
  var markedForDelete: mutable.Map[TimePoint, Set[GroundRule]] = mutable.Map()
  var reverseDeleteMap: mutable.Map[GroundRule, TimePoint] = mutable.Map()

  override def initialize(groundRules: Seq[GroundRule]) = groundRules foreach (x => tms.add(GroundRule.toNormalRule(x)))

  override def remove(timePoint: TimePoint)(rules: Seq[GroundRule]): Unit = {
    rules foreach markAsDeleted(timePoint)
  }

  override def getModel(timePoint: TimePoint): Result = Result(tms.getModel())

  override def add(timePoint: TimePoint)(rules: Seq[GroundRule]): Unit = {
    val markedAsDeleteEntries = reverseDeleteMap filter (x => rules.contains(x._1))
    // We don't need to add these rules - instead don't remove them
    markedAsDeleteEntries foreach (x => unmarkAsDeleted(x._1, x._2))

    val newRules = rules filterNot markedAsDeleteEntries.contains

    newRules foreach (x => tms.add(GroundRule.toNormalRule(x)))

    removeExpiredRules(timePoint)
  }

  def markAsDeleted(timePoint: TimePoint)(rule: GroundRule) = {
    reverseDeleteMap(rule) = timePoint

    val r = markedForDelete.getOrElse(timePoint, Set()) + rule

    markedForDelete.update(timePoint, r)
  }

  def unmarkAsDeleted(rule: GroundRule, timePoint: TimePoint) = {
    reverseDeleteMap.remove(rule)

    val notDeleted = markedForDelete.getOrElse(timePoint, Set()) - rule
    if (notDeleted.isEmpty)
      markedForDelete.remove(timePoint)
    else
      markedForDelete(timePoint) = notDeleted
  }

  def removeExpiredRules(timePoint: TimePoint): Unit = {
    // to list assures we are not reevaluating the iterable during remove
    val expiredTimePoints: List[TimePoint] = markedForDelete.keys filter (_.value < timePoint.value - laziness) toList

    expiredTimePoints.foreach(t => {
      val rules = markedForDelete.remove(t).get
      rules foreach (r => {
        tms.remove(GroundRule.toNormalRule(r))
        reverseDeleteMap remove (r)
      })
    })
  }
}
