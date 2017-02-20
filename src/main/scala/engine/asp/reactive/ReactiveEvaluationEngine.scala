package engine.asp.reactive

import clingo.reactive.{ReactiveClingo, ReactiveClingoProgram, Tick}
import clingo.{ClingoEvaluation, ClingoWrapper}
import common.Resource
import core._
import core.asp.{AspFact, NormalRule}
import core.lars.TimePoint
import engine.asp._
import engine._

/**
  * Created by FM on 18.05.16.
  */
case class ReactiveEvaluationEngine(program: LarsProgramEncoding, clingoWrapper: ClingoWrapper = ClingoWrapper()) extends EvaluationEngine with Resource {

  val clingoProgram: ReactiveClingoProgram = ReactiveClingoProgram.fromMapped(program)
  val reactiveClingo = new ReactiveClingo(clingoWrapper)

  val runningReactiveClingo = reactiveClingo.executeProgram(clingoProgram)

  val atomTracker = AtomTracking(program.maximumTimeWindowSizeInTicks, program.maximumTupleWindowSize, (g, t, p) => TrackedAtomWithClingo(g, t, p))

  def close() = runningReactiveClingo.close

  override def append(time: TimePoint)(atoms: Atom*): Unit = {

    val groundAtoms = trackAtoms(time, atoms)

    runningReactiveClingo.signal(groundAtoms map (_.clingoArgument))
  }

  override def evaluate(time: TimePoint): Result = {

    val parameters = Seq(
      Tick(clingoProgram.timeDimension.parameter, time.value),
      Tick(clingoProgram.countDimension.parameter, atomTracker.tupleCount)
    )

    val clingoModel = runningReactiveClingo.evaluate(parameters)

    discardOutdatedAtoms(time)

    clingoModel match {
      case Some(model) => {
        //TODO add filtering for such that clingoModel contains only output stream
        val models: Set[Model] = model.map(_.map(ClingoEvaluation.convert))


        Result(Some(models.head.collect {
          case p: PinnedAtAtom if p.time == time => p.atom
        })) //pick first
      }
      case None => NoResult
    }
  }

  private def discardOutdatedAtoms(time: TimePoint) = {
    val atomsToRemove = atomTracker.discardOutdatedAtoms(time)

    runningReactiveClingo.expire(atomsToRemove.map(_.clingoArgument))
  }

  private def trackAtoms(time: TimePoint, atoms: Seq[Atom]) = {
    atomTracker.trackAtoms(time, atoms)
  }

  case class TrackedAtomWithClingo(atom: GroundAtom, time: TimePoint, position: Long) extends TrackedAtom {
    val timeDimension = Tick(clingoProgram.timeDimension.parameter, time.value)
    val cntDimension = Tick(clingoProgram.countDimension.parameter, position)
    val clingoArgument = (atom, Seq(timeDimension, cntDimension))
  }

}
