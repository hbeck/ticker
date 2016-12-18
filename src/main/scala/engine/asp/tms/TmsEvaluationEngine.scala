package engine.asp.tms

import core._
import core.asp.{AspFact, AspRule, NormalRule}
import core.lars.{GroundRule, Grounder, LarsProgramInspection, TimePoint}
import engine.asp._
import engine.asp.tms.policies.TmsPolicy
import engine.{EvaluationEngine, NoResult, Result, UnknownResult}

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluationEngine(pinnedAspProgram: PinnedProgramWithLars, tmsPolicy: TmsPolicy) extends EvaluationEngine {
  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)
  val convertToPinned = PinnedModelToLarsModel(pinnedAspProgram)

  val (groundRules, nonGroundRules) = incrementalProgram.rules partition (_.isGround)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(groundRules)

  //book keeping for auxiliary atoms to handle window logic
  var tuplePositions: List[Atom] = List()
  var signalStream: Map[TimePoint, Set[NormalRule]] = Map()
  var fluentAtoms: Map[(Predicate, Seq[Argument]), AtomWithArgument] = Map()

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    trackAuxiliaryAtoms(time, atoms)
    cachedResults(time) = prepare(time, atoms.toSet)
    discardOutdatedAuxiliaryAtoms(time)
  }

  def prepare(time: TimePoint, signalAtoms: Set[Atom]): Result = {
    val pin = Pin(time)

    val pinnedSignals = pin.ground(signalAtoms map pin.apply)

    // TODO: this bookkeeping should be done in trackAux
    signalStream = signalStream updated(time, pinnedSignals ++ signalStream.getOrElse(time, Set()))

    val allHistoricalSignals: Seq[NormalRule] = signalStream.values flatMap (_.toSeq) toSeq

    // performs simple pinning-calcuations (eg. T + 1)
    val groundTimeVariableCalculations = nonGroundRules map (r => pin.ground(r))

    val grounder = new GroundRule[NormalRule, Atom, Atom]
    val inspectWithAllSignals = LarsProgramInspection.from(groundTimeVariableCalculations ++ allHistoricalSignals)
    val grounded = groundTimeVariableCalculations flatMap grounder.ground(inspectWithAllSignals)

    // TODO discuss if we want this
    val signalsHoldingNow = signalAtoms map (AspFact(_)) toSeq

    val orderedTuples = deriveOrderedTuples()
    val fluentTuples = fluentAtoms.values.map(pin.apply)

    val nowAtom = pin.ground(Set(pin(engine.asp.now))) toSeq

    val add = tmsPolicy.add(time) _

    // separating the calls ensures maximum on support for rules
    // facts first
    add(nowAtom)
    add(signalsHoldingNow)
    add(pinnedSignals.toSeq)
    add(orderedTuples)
    //        add(fluentTuples)
    // then rules
    add(grounded)

    val model = tmsPolicy.getModel(time)

    val remove = tmsPolicy.remove(time) _

    // rules first
    remove(grounded)
    // then facts
    // we never remove extensional atoms explicitly (the policy might do it)
    remove(orderedTuples)
//    remove(fluentTuples)
    remove(signalsHoldingNow)
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
      case Some(m) => Result(Some(convertToPinned(time, asPinnedAtoms(m, time))))
      case None => NoResult
    }
  }

  def deriveOrderedTuples() = tuplePositions.zipWithIndex.
    map { v => v._1.asTupleReference(v._2) }.
    map(x => AspFact[Atom](x))

  def asPinnedAtoms(model: Model, timePoint: TimePoint): Set[PinnedAtom] = model map {
    case p: PinnedAtom => p
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimeValue)) => PinnedAtom(GroundAtom(p), t.timePoint)
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)
  }

  def discardOutdatedAuxiliaryAtoms(time: TimePoint) = {
    val maxWindowTicks = pinnedAspProgram.maximumWindowSize.ticks(pinnedAspProgram.tickSize)
    tuplePositions = tuplePositions.take(pinnedAspProgram.maximumTupleWindowSize.toInt)

    val atomsToRemove = signalStream filterKeys (t => t.value < time.value - maxWindowTicks)
    signalStream = signalStream -- atomsToRemove.keySet

    atomsToRemove foreach { case (timePoint, signals) => tmsPolicy.remove(timePoint)(signals toSeq) }
  }

  private def trackAuxiliaryAtoms(time: TimePoint, atoms: Seq[Atom]) = {
    tuplePositions = atoms.toList ++ tuplePositions
    fluentAtoms = atoms.foldLeft(fluentAtoms)((m, a) => m updated(asFluentMap(a), a.asFluentReference()))
    //    stream = stream.updated(time, atoms.toSet ++ stream.getOrElse(time, Set()))
  }

  def asFluentMap(atom: Atom) = {
    val arguments = atom match {
      case aa: AtomWithArgument => aa.arguments take 1
      case _ => Seq()
    }

    (atom.predicate, arguments)
  }
}
