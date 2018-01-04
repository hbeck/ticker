package reasoner.asp

import core.Atom
import core.lars.TimePoint
import reasoner._
import reasoner.common.SignalTracker

/**
  * Created by FM on 21.04.16.
  */

case class AspPullReasoner(val clingoEvaluation: ClingoEvaluation) extends AspReasoner {

  val signalTracker = SignalTracker(clingoEvaluation.program)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  def prepare(time: TimePoint) = {

    val result = clingoEvaluation(time, signalTracker.tupleCount, signalTracker.allTimePoints(time).toSet)

    cachedResults.put(time, result)
  }

  def evaluate(time: TimePoint) = {
    if (!cachedResults.contains(time))
      prepare(time)

    signalTracker.discardOutdatedSignals(time)

    cachedResults(time)
  }

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    signalTracker.track(time, atoms)
    cachedResults.remove(time)
  }
}
