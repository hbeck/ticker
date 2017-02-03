package engine.asp.incremental

import clingo.reactive.{ReactiveClingo, ReactiveClingoProgram, Tick}
import clingo.{ClingoEvaluation, ClingoWrapper}
import core._
import core.asp.{AspFact, NormalRule}
import core.lars.TimePoint
import engine.asp._
import engine.{EvaluationEngine, Result}

/**
  * Created by FM on 18.05.16.
  */
case class ReactiveEvaluationEngine(program: TransformedLarsProgram, clingoWrapper: ClingoWrapper = ClingoWrapper()) extends EvaluationEngine {

  val clingoProgram = ReactiveClingoProgram.fromMapped(program)
  val reactiveClingo = new ReactiveClingo(clingoWrapper)

  val runningReactiveClingo = reactiveClingo.executeProgram(clingoProgram)

  //  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()


  //book keeping for auxiliary atoms to handle window logic
  var tuplePositions: List[Atom] = List()
  var signalStream: Map[TimePoint, Set[NormalRule]] = Map()
  var fluentAtoms: Map[(Predicate, Seq[Argument]), AtomWithArgument] = Map()

  def terminate = runningReactiveClingo.terminate

  override def append(time: TimePoint)(atoms: Atom*): Unit = {

    //    cachedResults(time) = prepare(time, atoms.toSet)
    //    discardOutdatedAuxiliaryAtoms(time)


    val groundAtoms = atoms.zipWithIndex map {a => (
      GroundAtom(a._1.predicate), //TODO hb? arguments
      Seq(
        Tick(clingoProgram.timeConstraint.parameter, time.value),
        Tick(clingoProgram.countConstraint.parameter, tuplePositions.size + a._2 + 1)
      )
    )}

    trackAuxiliaryAtoms(time, atoms)
    runningReactiveClingo.signal(groundAtoms)

  }

  def prepare(time: TimePoint, signalAtoms: Set[Atom]): Result = {
    Result(None)
    //    val pin = Pin(time)
    //
    //    val pinnedSignals = pin.ground(signalAtoms map pin.apply)
    //
    //    // TODO: this bookkeeping should be done in trackAux
    //    signalStream = signalStream updated(time, pinnedSignals ++ signalStream.getOrElse(time, Set()))
    //
    //    // TODO hb: seems crazy to always create the entire sequency from scratch instead of updating a data structure
    //    // (we have three iterations over all values instead of a single addition of the new atoms;
    //    //  maybe we should use a data structure that maintains signalStream and allHistoricalSignals?)
    //    val allHistoricalSignals: Seq[NormalRule] = signalStream.values flatMap (_.toSeq) toSeq
    //
    //    // performs simple pinning-calculations (eg. T + 1)
    //    val groundTimeVariableCalculations = nonGroundRules map (r => pin.ground(r))
    //
    //    val grounder = new GroundRule[NormalRule, Atom, Atom]()
    //    val inspectWithAllSignals = LarsProgramInspection.from(groundTimeVariableCalculations ++ allHistoricalSignals)
    //    val grounded = groundTimeVariableCalculations flatMap grounder.ground(inspectWithAllSignals)
    //
    //    // TODO discuss if we want this
    //    val signalsHoldingNow = signalAtoms map (AspFact(_)) toSeq
    //
    //    val orderedTuples = deriveOrderedTuples()
    //    val fluentTuples = fluentAtoms.values.map(pin.apply)
    //
    //    val nowAtom = pin.ground(Set(pin(engine.asp.now))) toSeq
    //
    //    val add = tmsPolicy.add(time) _
    //
    //    // separating the calls ensures maximum on support for rules
    //    // facts first
    //    add(nowAtom)
    //    add(signalsHoldingNow)
    //    add(pinnedSignals.toSeq)
    //    add(orderedTuples)
    //    //        add(fluentTuples)
    //    // then rules
    //    add(grounded)
    //
    //    val model = tmsPolicy.getModel(time)
    //
    //    val remove = tmsPolicy.remove(time) _
    //
    //    // rules first
    //    remove(grounded)
    //    // then facts
    //    // we never remove extensional atoms explicitly (the policy might do it)
    //    remove(orderedTuples)
    //    //    remove(fluentTuples)
    //    remove(signalsHoldingNow)
    //    remove(nowAtom)
    //
    //    model
  }

  override def evaluate(time: TimePoint): Result = {

   val model =  runningReactiveClingo.evaluate( Seq(
      Tick(clingoProgram.timeConstraint.parameter, time.value),
      Tick(clingoProgram.countConstraint.parameter, tuplePositions.size )
    ))
val mapped = model.get.map(m => m.map(ClingoEvaluation.convert))

    Result.apply(Some(mapped.head))
  }

  def deriveOrderedTuples() = tuplePositions.zipWithIndex.
    map { v => v._1.asTupleReference(v._2) }.
    map(x => AspFact[Atom](x))

  def asPinnedAtoms(model: Model, timePoint: TimePoint): Set[PinnedAtom] = model map {
    case p: PinnedAtAtom => p
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimePoint)) => ConcretePinnedAtAtom(GroundAtom(p), t)
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)
  }

  def discardOutdatedAuxiliaryAtoms(time: TimePoint) = {
    //    val maxWindowTicks = pinnedAspProgram.maximumWindowSize.ticks(pinnedAspProgram.tickSize)
    //    tuplePositions = tuplePositions.take(pinnedAspProgram.maximumTupleWindowSize.toInt)
    //
    //    val atomsToRemove = signalStream filterKeys (t => t.value < time.value - maxWindowTicks)
    //    signalStream = signalStream -- atomsToRemove.keySet
    //
    //    atomsToRemove foreach { case (timePoint, signals) => tmsPolicy.remove(timePoint)(signals toSeq) }
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
