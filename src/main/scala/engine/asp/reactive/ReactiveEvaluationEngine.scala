package engine.asp.reactive

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

  val clingoProgram: ReactiveClingoProgram = ReactiveClingoProgram.fromMapped(program)
  val reactiveClingo = new ReactiveClingo(clingoWrapper)

  val runningReactiveClingo = reactiveClingo.executeProgram(clingoProgram)

  //book keeping for auxiliary atoms to handle window logic
  var tuplePositions: List[TrackedAtom] = List()
  var signalStream: Map[TimePoint, Seq[TrackedAtom]] = Map()

  def terminate = runningReactiveClingo.terminate

  override def append(time: TimePoint)(atoms: Atom*): Unit = {

    val groundAtoms = trackAtoms(time, atoms)

    runningReactiveClingo.signal(groundAtoms map (_.clingoArgument))

  }

  override def evaluate(time: TimePoint): Result = {

    val parameters = Seq(
      Tick(clingoProgram.timeDimension.parameter, time.value),
      Tick(clingoProgram.countDimension.parameter, tuplePositions.size)
    )

    val clingoModel = runningReactiveClingo.evaluate(parameters)

    discardOutdatedAtoms(time)

    clingoModel match {
      case Some(model) => {
        //TODO add filtering for such that clingoModel contains only output stream
        val models: Set[Model] = model.map(_.map(ClingoEvaluation.convert))

        Result(Some(models.head)) //pick first
      }
      case None => NoResult
    }
  }


  private def discardOutdatedAtoms(time: TimePoint) = {
    // TODO: this is not correct (includes all kind of windows, not only time)
    val maxWindowTicks = program.larsRuleEncodings.
      flatMap(_.windowAtomEncoders).
      map(_.length).
      max

    tuplePositions = tuplePositions.take(program.maximumTupleWindowSize.toInt)

    val atomsToRemove = signalStream filterKeys (t => t.value < time.value - maxWindowTicks)
    signalStream = signalStream -- atomsToRemove.keySet


    // TODO: explicit expire also for tuple-positions?
    atomsToRemove foreach {
      case (_, signals) => runningReactiveClingo.expire(signals.map(_.clingoArgument))
    }
  }

  private def trackAtoms(time: TimePoint, atoms: Seq[Atom]) = {

    val trackedAtoms = atoms.zipWithIndex map { case (atom, position) =>
      TrackedAtom(
        GroundAtom.assertGround(atom),
        Tick(clingoProgram.timeDimension.parameter, time.value),
        Tick(clingoProgram.countDimension.parameter, tuplePositions.size + position + 1) //zip begins with 0, hence + 1

      )
    }

    tuplePositions = tuplePositions ++ trackedAtoms.toList
    signalStream = signalStream.updated(time, trackedAtoms ++ signalStream.getOrElse(time, Seq()))

    trackedAtoms
  }


  case class TrackedAtom(groundAtom: GroundAtom, time: Tick, position: Tick) {
    val clingoArgument = (groundAtom, Seq(time, position))
  }

}
