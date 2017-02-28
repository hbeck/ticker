package engine.asp.tms

import core._
import core.asp.{AspFact, AspRule, NormalRule}
import core.lars.{Assignment, GroundRule, LarsProgramInspection, TimePoint}
import engine._
import engine.asp._
import engine.asp.tms.policies.TmsPolicy

/**
  * Created by FM on 18.05.16.
  */
case class IncrementalEvaluationEngine(pinnedAspProgram: LarsProgramEncoding, tmsPolicy: TmsPolicy) extends EvaluationEngine {

  val (groundRules, nonGroundRules) = pinnedAspProgram.baseRules.
    toSet.
    map(PinnedAspToIncrementalAsp.stripTickAtoms).
    partition(_.isGround)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(groundRules.toSeq)

  val atomTracker = new AtomTracking(pinnedAspProgram.maximumTimeWindowSizeInTicks, pinnedAspProgram.maximumTupleWindowSize, DefaultTrackedAtom.apply)

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    cachedResults(time) = prepare(time, atoms)
    discardOutdatedAuxiliaryAtoms(time)
  }

  private def asFact(t: TrackedAtom): Seq[NormalRule] = Seq(t.timePinned, t.countPinned, t.timeCountPinned).map(AspFact[Atom](_))

  def prepare(time: TimePoint, signalAtoms: Seq[Atom]): Result = {

    val previousTupleCount = atomTracker.tupleCount

    val tracked = atomTracker.trackAtoms(time, signalAtoms)
    val pinnedSignals = tracked.flatMap(asFact)

    // TODO hb: seems crazy to always create the entire sequence from scratch instead of updating a data structure
    // (we have three iterations over all values instead of a single addition of the new atoms;
    //  maybe we should use a data structure that maintains signalStream and allHistoricalSignals?)
    val allHistoricalSignals: Set[NormalRule] = atomTracker.allTimePoints(time).flatMap(asFact).toSet

    val pin = Pin(
      Assignment(
        Map(
          core.lars.T -> time,
          core.lars.C -> IntValue(atomTracker.tupleCount.toInt)
        )
      )
    )

    // performs simple pinning-calculations (eg. T + 1)
    val groundTimeVariableCalculations = nonGroundRules map (r => pin.ground(r))

    // we need to call incremental rules at least one time (if no atoms stream in we use the current tuple-count)
    val countsToIterate = if (atomTracker.tupleCount == previousTupleCount)
      Seq(atomTracker.tupleCount)
    else
      (previousTupleCount + 1) to atomTracker.tupleCount

    val incrementalRules = pinnedAspProgram.incrementalEncoders.flatMap {
      encoder => countsToIterate map (c => encoder.incrementalRulesAt(CurrentPosition(time, c)))
    }


    // TODO: there might be some optimization: for small tuple-based windows and multiple atoms at a single timepoint
    // 1 -> {a,b,c}
    // incrementalRulesAt(1, 1), incrementalRulesAt(1,2), incrementalRulesAt(1,3)
    // d :-  w #2 d a

    val (incrementalAdd, incrementalRemove) = incrementalRules.foldLeft((Set[NormalRule](), Set[NormalRule]()))((v, r) => (v._1 ++ r.toAdd, v._2 ++ r.toRemove))

    // TODO: this hack is needed for grounding @
    val incrementalFacts = incrementalAdd.filter(_.isGround).map(f => AspFact(f.head))

    val grounder = new GroundRule[NormalRule, Atom, Atom]()
    val inspectWithAllSignals = LarsProgramInspection.from((groundTimeVariableCalculations ++ allHistoricalSignals ++ incrementalAdd ++ incrementalFacts).toSeq)
    // TODO: grounding fails here
    val grounded = (groundTimeVariableCalculations ++ incrementalAdd) flatMap grounder.ground(inspectWithAllSignals)

    val add = tmsPolicy.add(time) _
    val remove = tmsPolicy.remove(time) _

    // separating the calls ensures maximum on support for rules
    // facts first
    add(pinnedSignals)
    add(grounded toSeq)


    remove(incrementalRemove.filter(_.isGround).toSeq)

    val model = tmsPolicy.getModel(time)

    // TODO: this needs to be fixed
    // grounded contains also the incremental-Rules (because they might need grounding too)
    // we don't want to remove grounded incremental-Rules, because they are removed later anyway
    val rulesToRemove = grounded -- incrementalAdd

    remove(rulesToRemove toSeq)

    model
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

    // TODO: model-cleaning is still needed (remote e.g a_at(t), filter only for current timepoint, ..)
    resultingModel match {
      case Some(m) => Result(Some(m))
      case None => NoResult
    }
  }

  def discardOutdatedAuxiliaryAtoms(time: TimePoint) = {
    val atomsToRemove = atomTracker.discardOutdatedAtoms(time)

    atomsToRemove foreach { atom => tmsPolicy.remove(atom.time)(asFact(atom)) }
  }
}
