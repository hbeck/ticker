package engine.asp.incremental

import clingo.reactive.{ReactiveClingo, ReactiveClingoProgram, Tick}
import clingo.{ClingoEvaluation, ClingoWrapper}
import core._
import core.asp.{AspFact, NormalRule}
import core.lars.TimePoint
import engine.asp._
import engine.{EvaluationEngine, NoResult, Result}

/**
  * Created by FM on 18.05.16.
  */
case class ReactiveEvaluationEngine(program: LarsProgramEncoding, clingoWrapper: ClingoWrapper = ClingoWrapper()) extends EvaluationEngine {

  val clingoProgram = ReactiveClingoProgram.fromMapped(program)
  val reactiveClingo = new ReactiveClingo(clingoWrapper)

  val runningReactiveClingo = reactiveClingo.executeProgram(clingoProgram)

  //  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()


  //book keeping for auxiliary atoms to handle window logic
  var tuplePositions: List[Atom] = List() //TODO we do not want to keep entire history!
  var signalStream: Map[TimePoint, Set[NormalRule]] = Map()
  var fluentAtoms: Map[(Predicate, Seq[Argument]), AtomWithArgument] = Map()

  def terminate = runningReactiveClingo.terminate

  override def append(time: TimePoint)(atoms: Atom*): Unit = {

    //    cachedResults(time) = prepare(time, atoms.toSet)
    //    discardOutdatedAuxiliaryAtoms(time)

    val groundAtoms = atoms.zipWithIndex map { case (atom, position) =>
      (
        GroundAtom.assertGround(atom),
        Seq(
          Tick(clingoProgram.timeDimension.parameter, time.value),
          Tick(clingoProgram.countDimension.parameter, tuplePositions.size + position + 1) //zip begins with 0, hence + 1
        )
      )
    }

    trackAuxiliaryAtoms(time, atoms) //TODO hb review
    runningReactiveClingo.signal(groundAtoms) //TODO hb review

  }

  override def evaluate(time: TimePoint): Result = {

    val clingoModel = runningReactiveClingo.evaluate(Seq(
      Tick(clingoProgram.timeDimension.parameter, time.value),
      Tick(clingoProgram.countDimension.parameter, tuplePositions.size)
    ))

    clingoModel match {
      case Some(model) => {
        //TODO add filtering for such that clingoModel contains only output stream
        val models: Set[Model] = model.map(_.map(ClingoEvaluation.convert))

        Result(Some(models.head)) //pick first
      }
      case None => NoResult
    }
  }

  def deriveOrderedTuples() = tuplePositions.zipWithIndex.
    map(v => v._1.asTupleReference(v._2)).
    map(x => AspFact[Atom](x))

  def asPinnedAtoms(model: Model, timePoint: TimePoint): Set[PinnedAtom] = model map {
    case p: PinnedAtAtom => p
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => PinnedAtom(a, timePoint)
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

  //TODO hb review what does this do?
  private def trackAuxiliaryAtoms(time: TimePoint, atoms: Seq[Atom]) = {
    tuplePositions = atoms.toList ++ tuplePositions
    //fluentAtoms = atoms.foldLeft(fluentAtoms)((m, a) => m updated(asFluentMap(a), a.asFluentReference()))
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
