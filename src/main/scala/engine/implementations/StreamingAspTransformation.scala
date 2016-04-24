package engine.implementations

import asp.{Asp, AspConversion, AspExpression}
import core.{Atom, AtomWithArguments, Fact}
import engine.{Atom, Result, Time}

/**
  * Created by FM on 22.04.16.
  */
object StreamingAspTransformation {
  val now = Atom("now")

  def transform(time: Time, atoms: Set[Atom]) = {
    val nowAtT = now(time.milliseconds.toString)

    val atomFacts = (atoms + nowAtT) map (x => Fact(x))

    atomFacts map (x => AspConversion(x))
  }
}

case class StreamingAspTransformation(aspExpressions: Set[AspExpression], aspEngine: Asp = Asp()) {

  def prepare(time: Time, atoms: Set[Atom]): Result = {

    val transformed = StreamingAspTransformation.transform(time, atoms)

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
