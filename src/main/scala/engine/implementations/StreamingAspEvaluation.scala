package engine.implementations

import clingo._
import clingo.{ClingoExpression, ClingoProgram}
import core.asp.AspFact
import core.lars.TimePoint
import core.{Atom, AtomWithArguments}
import engine._

/**
  *
  * Created by FM on 22.04.16.
  */
object StreamingAspEvaluation {

  def transform(dataStream: Stream): Set[ClingoExpression] = {
    dataStream flatMap (x => transformAtoms(x.time, x.atoms))
  }

  def atomAtT(time: TimePoint, atom: Atom) = {
    val timeParameter = time.timePoint.toString

    AspFact(atom(timeParameter))
  }

  def transformAtoms(time: TimePoint, atoms: Set[Atom]) = {
    val atomsWithT = atoms.map(x => atomAtT(time, x))

    atomsWithT map (x => ClingoConversion(x))
  }

  def transform(currentTime: TimePoint, dataStream: Stream): Set[ClingoExpression] = {
    val transformedAtoms = transform(dataStream)

    val transformedAtomsAndNow = transformedAtoms + ClingoConversion(atomAtT(currentTime, PlainLarsToAsp.now))

    transformedAtomsAndNow
  }
}

case class StreamingAspEvaluation(aspExpressions: ClingoProgram, aspEngine: ClingoEvaluation = ClingoEvaluation()) extends AspEvaluation {

  def prepare(time: TimePoint, dataStream: Stream): Result = {

    val transformed = StreamingAspEvaluation.transform(time, dataStream)

    val aspResult = aspEngine(aspExpressions ++ transformed).headOption

    val result = aspResult match {
      case Some(model) => {
        val atoms = model.filterNot {
          case AtomWithArguments(baseAtom, _) => baseAtom == PlainLarsToAsp.now
          case _ => false
        }
        Some(atoms)
      }
      case None => None
    }

    new Result {
      override def get: Option[Set[Atom]] = result
    }
  }
}
