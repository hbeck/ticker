package engine.asp.tms.policies

import core.asp._
import core.lars.{Duration, TimePoint}
import engine.Result
import engine.asp.GroundAspRule
import engine.asp.tms.GroundRule
import jtms.{Jtms, JtmsGreedy, JtmsUpdateAlgorithm}

import scala.collection.mutable

/**
  * Created by FM on 12.06.16.
  */
case class LazyRemovePolicy(tms: JtmsUpdateAlgorithm = JtmsGreedy(), laziness: Duration = 0) extends TmsPolicy {

  val markedForDelete: mutable.Map[TimePoint, Set[NormalRule]] = mutable.Map()
  val reverseDeleteMap: mutable.Map[NormalRule, TimePoint] = mutable.Map()

  override def initialize(groundRules: Seq[NormalRule]) = groundRules foreach (x => tms.add(x))

  override def remove(timePoint: TimePoint)(rules: Seq[NormalRule]): Unit = {
    rules foreach markAsDeleted(timePoint)
  }

  override def getModel(timePoint: TimePoint): Result = Result(tms.getModel())

  override def add(timePoint: TimePoint)(rules: Seq[NormalRule]): Unit = {
    val markedAsDeleteEntries = reverseDeleteMap filter { case (rule,_) => rules contains rule }
    // We don't need to add these rules - instead don't remove them
    markedAsDeleteEntries foreach { case (rule,timePoint) => unmarkAsDeleted(rule,timePoint) }

    val newRules = rules filterNot markedAsDeleteEntries.contains
    newRules foreach (x => tms.add(x))

    removeExpiredRules(timePoint)
  }

  def markAsDeleted(timePoint: TimePoint)(rule: NormalRule) = {
    reverseDeleteMap(rule) = timePoint

    val r = markedForDelete.getOrElse(timePoint, Set()) + rule

    markedForDelete.update(timePoint, r)
  }

  def unmarkAsDeleted(rule: NormalRule, timePoint: TimePoint) = {
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
        tms.remove(r)
        reverseDeleteMap remove (r)
      })
    })
  }
}
