package engine.asp.tms

import core._
import core.asp.{AspFact, AspRule}
import core.lars.TimePoint
import engine.asp._
import engine.asp.tms.policies.TmsPolicy
import engine.{EvaluationEngine, NoResult, Result, UnknownResult}

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluationEngine(pinnedAspProgram: PinnedProgramWithLars, tmsPolicy: TmsPolicy) extends EvaluationEngine {
  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)

  val (groundRules, nonGroundRules) = incrementalProgram.rules partition (_.isGround)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(groundRules.map(x => GroundedNormalRule(x)))

  //book keeping for auxiliary atoms to handle window logic
  var tuplePositions: List[Atom] = List()
  var stream: Map[TimePoint, GroundedAspStream] = Map()
  var fluentAtoms: Map[(Predicate, Seq[Argument]), AtomWithArgument] = Map()

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    tuplePositions = atoms.toList ++ tuplePositions
    fluentAtoms = atoms.foldLeft(fluentAtoms)((m, a) => m updated (asFluentMap(a), a.asFluentReference()))
    cachedResults(time) = prepare(time, atoms.toSet)
    discardOutdatedAuxiliaryAtoms(time)
  }

  def prepare(time: TimePoint, signalAtoms: Set[Atom]): Result = {
    val pin = Pin(time)

    val pinnedGroundedRules = pin.ground(nonGroundRules)
    val pinnedSignals = pin.ground(signalAtoms map pin.apply)

    val orderedTuples = deriveOrderedTuples
    val fluentTuples = fluentAtoms.values.map(pin.ground).map(AspFact(_)).toSeq

    val add = tmsPolicy.add(time) _

    // separating the calls ensures maximum on support for rules
    // facts first
    add(pinnedSignals.toSeq)
    add(orderedTuples)
    add(fluentTuples)
    // then rules
    add(pinnedGroundedRules)

    val model = tmsPolicy.getModel(time)

    val remove = tmsPolicy.remove(time) _

    // rules first
    remove(pinnedGroundedRules)
    // then facts
    // we never remove extensional atoms explicitly (the policy might do it)
    remove(orderedTuples)
    remove(fluentTuples)

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
      case Some(m) => Result(Some(PinnedModelToLarsModel(time, asPinnedAtoms(m, time))))
      case None => NoResult
    }
  }

  def deriveOrderedTuples = tuplePositions.zipWithIndex.
    map { v => v._1.asTupleReference(v._2) }.
    map(x => GroundedNormalRule(AspRule(x)))

  def asPinnedAtoms(model: Model, timePoint: TimePoint): Set[PinnedAtom] = model map {
    case p: PinnedAtom => p
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimeValue)) => PinnedAtom(GroundAtom(p), t.timePoint)
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)
  }

  def discardOutdatedAuxiliaryAtoms(time: TimePoint) = {

    tuplePositions = tuplePositions.take((pinnedAspProgram.maximumWindowSize).toInt)

    val atomsToRemove = stream filterKeys (t => t.value < time.value - pinnedAspProgram.maximumWindowSize)
    stream = stream -- atomsToRemove.keySet

    atomsToRemove foreach { case (timePoint,signals) => tmsPolicy.remove(timePoint)(signals.toSeq) }
  }

  def asFluentMap(atom: Atom) = {
    val arguments = atom match {
      case aa: AtomWithArgument => aa.arguments take 1
      case _ => Seq()
    }

    (atom.predicate, arguments)
  }
}
