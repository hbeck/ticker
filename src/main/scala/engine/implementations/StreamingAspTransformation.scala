package engine.implementations

import asp.{Asp, AspConversion, AspExpression, AspExpressionProgram}
import core.{Atom, AtomWithArguments, AspFact}
import engine.{Atom, Result, Stream, Time}

/**
  * Created by FM on 22.04.16.
  */
object StreamingAspTransformation {
  val now = Atom("now")


  def transform(dataStream: Stream): Set[AspExpression] = {
    dataStream flatMap (x => transformAtoms(x.time, x.atoms))
  }

  def atomAtT(time: Time, atom: Atom) = {
    val timeParameter = time.timePoint.toString

    AspFact(atom(timeParameter))
  }

  def transformAtoms(time: Time, atoms: Set[Atom]) = {
    val atomsWithT = atoms.map(x => atomAtT(time, x))

    atomsWithT map (x => AspConversion(x))
  }

  def transform(currentTime: Time, dataStream: Stream): Set[AspExpression] = {
    val transformedAtoms = transform(dataStream)

    val transformedAtomsAndNow = transformedAtoms + AspConversion(atomAtT(currentTime, now))

    // TODO: do we need the last clause?
    transformedAtomsAndNow ++ (dataStream flatMap (x => x.atoms.map(AspFact(_))) map (x => AspConversion(x)))
  }
}

case class StreamingAspTransformation(aspExpressions: AspExpressionProgram, aspEngine: Asp = Asp()) extends AspEvaluation {

  def prepare(time: Time, dataStream: Stream): Result = {

    val transformed = StreamingAspTransformation.transform(time, dataStream)

    val aspResult = aspEngine(aspExpressions ++ transformed).headOption

    // TODO: should we remove time(now)?
    // TODO: how do we differentiate if the model is defined or not? Adding the fact now(T) leads to a model
    val result = aspResult match {
      case Some(model) => {
        val atoms = model.filterNot {
          case AtomWithArguments(baseAtom, _) => baseAtom == StreamingAspTransformation.now
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
