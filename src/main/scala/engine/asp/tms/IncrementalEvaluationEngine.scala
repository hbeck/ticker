package engine.asp.tms

import core._
import core.asp.{AspFact, NormalRule}
import core.lars.{LarsProgramInspection, RuleGrounder, TimePoint}
import engine._
import engine.asp._
import engine.asp.tms.policies.TmsPolicy

import scala.collection.immutable.HashMap

/**
  * Created by FM on 18.05.16.
  */
case class IncrementalEvaluationEngine(larsProgramEncoding: LarsProgramEncoding, tmsPolicy: TmsPolicy) extends EvaluationEngine {

  //TODO curr
  /*
     create special incremental semantic class that wraps larsProgramEncoding.
     this class is responsible for delivering the new rules for grounding,
     and the old ones based on the ticks passed

     - to we ensure by the policy etc that prepare is called for each time point?
       otherwise, we have to explicitly take care of the sequence
   */

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(larsProgramEncoding.groundBaseRules)

  //book keeping for auxiliary atoms to handle window logic
  var tuplePositions: List[Atom] = List()

  val tracker = new AtomTracking(larsProgramEncoding.maximumTimeWindowSizeInTicks, larsProgramEncoding.maximumTupleWindowSize, DefaultTrackedAtom.apply)

  var rulesOutdatedAtTime: Map[Int,Set[NormalRule]] = HashMap[Int,Set[NormalRule]]()
  var rulesOutdatedAtCnt: Map[Long,Set[NormalRule]] = HashMap[Long,Set[NormalRule]]()

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    trackAuxiliaryAtoms(time, atoms)
    cachedResults(time) = prepare(time, atoms)
    discardOutdatedAuxiliaryAtoms(time)
  }

  private def asFact(t: TrackedAtom): Seq[NormalRule] = Seq(t.timePinned, t.countPinned, t.timeCountPinned).map(AspFact[Atom](_))

  def prepare(time: TimePoint, signalAtoms: Seq[Atom]): Result = {

    val previousTupleCount = tracker.tupleCount
    val trackedAtoms = tracker.trackAtoms(time, signalAtoms)
    val currentTupleCount = tracker.tupleCount

    val pinnedSignals = trackedAtoms flatMap asFact

    // TODO hb: updating instead of recomputing
    // (we have three iterations over all values instead of a single addition of the new atoms;
    //  maybe we should use a data structure that maintains signalStream and entireStreamAsFacts?)
    val entireStreamAsFacts: Set[NormalRule] = tracker.allTimePoints(time).flatMap(asFact).toSet

    val prevPosition = TickPosition(time,previousTupleCount) //TODO do we have to include time?
    val currPosition = TickPosition(time,currentTupleCount)

    val rulesToGround = larsProgramEncoding.rulesToGround(prevPosition,currPosition)

    //TODO incrementally update inspection
    val inspection = LarsProgramInspection.from(rulesToGround ++ entireStreamAsFacts)
    val preparedRuleGrounder = new RuleGrounder[NormalRule,Atom,Atom]().ground(inspection) _
    val groundedRulesToAdd = rulesToGround flatMap preparedRuleGrounder

    val add = tmsPolicy.add(time) _
    val remove = tmsPolicy.remove(time) _

    // separating the calls ensures maximum on support for rules
    // facts first
    add(pinnedSignals)
    add(groundedRulesToAdd)

    val outdatedRules = Set[NormalRule]()
    remove(rulesToRemove)

    //TODO update managing structure

    tmsPolicy.getModel(time)
  }

  override def evaluate(time: TimePoint): Result = {

    val resultingModel = cachedResults.get(time) match {
      case Some(result) => result.get
      case None => {
        if (cachedResults.nonEmpty && time.value < cachedResults.keySet.max.value) {
          return UnknownResult
        } else {
          prepare(time, Seq()).get
        }
      }
    }

    resultingModel match {
      case Some(m) => Result(Some(m))
      case None => NoResult
    }
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint): PinnedModel = model map {
    case p: PinnedAtAtom => p
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimePoint)) => GroundPinnedAtAtom(Atom(p), t)
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => PinnedAtom(a, timePoint)
  }

  def discardOutdatedAuxiliaryAtoms(time: TimePoint) = {
    //TODO current !!!
    //val maxWindowTicks = pinnedAspProgram.maximumWindowSize.ticks(pinnedAspProgram.tickSize)
    val maxWindowTicks = 100
    //TODO
    val atomsToRemove = tracker.discardOutdatedAtoms(time)

    atomsToRemove foreach { atom => tmsPolicy.remove(atom.time)(asFact(atom)) }
  }

  private def trackAuxiliaryAtoms(time: TimePoint, atoms: Seq[Atom]) = {
    tuplePositions = atoms.toList ++ tuplePositions
    //    stream = stream.updated(time, atoms.toSet ++ stream.getOrElse(time, Set()))
  }
}
