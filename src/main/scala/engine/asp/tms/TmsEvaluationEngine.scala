package engine.asp.tms

import core.{GroundAtom, _}
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


  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    cachedResults(time) = prepare(time, atoms.toSet)
  }

  def prepare(time: TimePoint, atoms: Set[Atom]): Result = {
    val pin = Pin(time)

    val groundedRules = pin.ground(nonGroundRules)
    // TODO: make it nicer
    val extensionalAtoms = pin.ground(pin.atoms(atoms))

    tmsPolicy.add(time)(groundedRules ++ extensionalAtoms)
    val model = tmsPolicy.getModel(time)
    // we never remove extensional atoms explicitly (the policy might do it)
    tmsPolicy.remove(time)(groundedRules)

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

  def asPinnedAtoms(model: Model, timePoint: TimePoint): Set[PinnedAtom] = model map {
    case p: PinnedAtom => p
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimeValue)) => p(t.timePoint)
    case g: Predicate => g(timePoint)
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)
  }
}
