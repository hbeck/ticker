package engine.asp.tms

import core._
import core.asp.AspRule
import core.lars.TimePoint
import engine.asp._
import engine.asp.tms.policies.TmsPolicy
import engine.{EvaluationEngine, Result, UnknownResult}

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluationEngine(pinnedAspProgram: MappedProgram, tmsPolicy: TmsPolicy) extends EvaluationEngine {
  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)

  val (groundRules, nonGroundRules) = incrementalProgram.rules partition (_.isGround)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(groundRules.map(x => GroundedNormalRule(x)))

  // TODO: wrong position? Move to Policy?
  var tuplePositions: List[Atom] = List()
  var extensionalAtomsStream: Map[TimePoint, GroundedStream] = Map()


  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    tuplePositions = atoms.toList ++ tuplePositions
    cachedResults(time) = prepare(time, atoms.toSet)
    trimByWindowSize(time)
  }

  def prepare(time: TimePoint, atoms: Set[Atom]): Result = {
    val pin = Pin(time)

    val groundedRules = pin.ground(nonGroundRules)
    // TODO: make it nicer
    val extensionalAtoms = pin.ground(pin.atoms(atoms))

    val orderedTuples = deriveOrderedTuples

    val add = tmsPolicy.add(time) _

    // separating the calls ensures maximum on support for rules
    add(extensionalAtoms.toSeq)
    add(orderedTuples)
    add(groundedRules)

    val model = tmsPolicy.getModel(time)

    val remove = tmsPolicy.remove(time) _

    remove(groundedRules)
    // we never remove extensional atoms explicitly (the policy might do it)
    remove(orderedTuples)

    model
  }

  override def evaluate(time: TimePoint): Result = {
    val resultingModel = cachedResults.get(time) match {
      case Some(result) => result.get
      case None => {
        //TODO think about this: We can't generate a result if the current time is in the past (of previous calculated time values)
        if (cachedResults.nonEmpty && time.value < cachedResults.keySet.max.value) {
          return UnknownResult
        } else {
          prepare(time, Set()).get
        }
      }
    }

    Result(Some(PinnedModelToLarsModel(time, asPinnedAtoms(resultingModel.get, time))))
  }

  def deriveOrderedTuples = tuplePositions.zipWithIndex.
    map { v => v._1.asTupleReference(v._2) }.
    map(x => GroundedNormalRule(AspRule(x)))

  def asPinnedAtoms(model: Model, timePoint: TimePoint): Set[PinnedAtom] = model map {
    case p: PinnedAtom => p
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimeValue)) => p(t.timePoint)
    case g: Predicate => g(timePoint)
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)
  }

  // TODO: move into policy?
  def trimByWindowSize(time: TimePoint) = {
    tuplePositions = tuplePositions.take((pinnedAspProgram.maximumWindowSize + 1).toInt)

    val atomsToRemove = extensionalAtomsStream.filterKeys(p => p.value < time.value - pinnedAspProgram.maximumWindowSize)
    extensionalAtomsStream = extensionalAtomsStream -- atomsToRemove.keySet

    atomsToRemove foreach (a => tmsPolicy.remove(a._1)(a._2.toSeq))
  }
}
