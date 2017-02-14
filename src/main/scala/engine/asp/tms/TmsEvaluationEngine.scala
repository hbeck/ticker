package engine.asp.tms

import core._
import core.asp.{AspFact, NormalFact, NormalRule}
import core.lars.{GroundRule, LarsProgramInspection, TimePoint}
import engine.asp._
import engine.asp.tms.policies.TmsPolicy
import engine.{EvaluationEngine, NoResult, Result, UnknownResult}

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluationEngine(pinnedAspProgram: LarsProgramEncoding, tmsPolicy: TmsPolicy) extends EvaluationEngine {

  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)
  val convertToPinned = PinnedModelToLarsModel(pinnedAspProgram)

  val (groundRules, nonGroundRules) = incrementalProgram.rules partition (_.isGround)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(groundRules)

  //book keeping for auxiliary atoms to handle window logic
  var tuplePositions: List[Atom] = List()
  var signalStream: Map[TimePoint, Set[NormalRule]] = Map()

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    trackAuxiliaryAtoms(time, atoms)
    cachedResults(time) = prepare(time, atoms.toSet)
    discardOutdatedAuxiliaryAtoms(time)
  }

  def prepare(time: TimePoint, signalAtoms: Set[Atom]): Result = {
    val pin = Pin(time)

    // TODO: Bookkeeping should be done differently
    // TODO: cnt-Atoms are currently missing
    val pinnedSignals: Set[NormalFact] = signalAtoms map (s => AspFact[Atom](PinnedAtom(s, time)))

    // TODO: this bookkeeping should be done in trackAux
        signalStream = signalStream updated(time, pinnedSignals ++ signalStream.getOrElse(time, Set()))

    // TODO hb: seems crazy to always create the entire sequency from scratch instead of updating a data structure
    // (we have three iterations over all values instead of a single addition of the new atoms;
    //  maybe we should use a data structure that maintains signalStream and allHistoricalSignals?)
    val allHistoricalSignals: Seq[NormalRule] = signalStream.values flatMap (_.toSeq) toSeq

    // performs simple pinning-calculations (eg. T + 1)
    val groundTimeVariableCalculations = nonGroundRules map (r => pin.ground(r))

    val grounder = new GroundRule[NormalRule, Atom, Atom]()
    val inspectWithAllSignals = LarsProgramInspection.from(groundTimeVariableCalculations ++ allHistoricalSignals)
    // TODO: grounding fails here
    val grounded = groundTimeVariableCalculations flatMap grounder.ground(inspectWithAllSignals)

    val nowAtom = pin.ground(Set(pin(engine.asp.now))) toSeq

    val add = tmsPolicy.add(time) _

    // separating the calls ensures maximum on support for rules
    // facts first
    add(nowAtom)
    add(pinnedSignals.toSeq)
    // then rules
    add(grounded)

    val model = tmsPolicy.getModel(time)

    val remove = tmsPolicy.remove(time) _

    // rules first
    remove(grounded)
    // then facts
    // we never remove extensional atoms explicitly (the policy might do it)
    remove(nowAtom)

    model
  }

  override def evaluate(time: TimePoint): Result = {

    val resultingModel = cachedResults.get(time) match {
      case Some(result) => result.get
      case None => {
        if (cachedResults.nonEmpty && time.value < cachedResults.keySet.max.value) {
          return UnknownResult
        } else {
          prepare(time, Set()).get
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
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimePoint)) => ConcretePinnedAtAtom(Atom(p), t)
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => PinnedAtom(a, timePoint)
  }

  def discardOutdatedAuxiliaryAtoms(time: TimePoint) = {
    //TODO current !!!
    //val maxWindowTicks = pinnedAspProgram.maximumWindowSize.ticks(pinnedAspProgram.tickSize)
    val maxWindowTicks = 100 //TODO
    tuplePositions = tuplePositions.take(pinnedAspProgram.maximumTupleWindowSize.toInt)

    val atomsToRemove = signalStream filterKeys (t => t.value < time.value - maxWindowTicks)
    signalStream = signalStream -- atomsToRemove.keySet

    atomsToRemove foreach { case (timePoint, signals) => tmsPolicy.remove(timePoint)(signals toSeq) }
  }

  private def trackAuxiliaryAtoms(time: TimePoint, atoms: Seq[Atom]) = {
    tuplePositions = atoms.toList ++ tuplePositions
    //    stream = stream.updated(time, atoms.toSet ++ stream.getOrElse(time, Set()))
  }
}
