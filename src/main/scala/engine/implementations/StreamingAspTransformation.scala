package engine.implementations

import asp.{Asp, AspConversion, AspExpression}
import core.{Atom, AtomWithArguments, Fact}
import engine.{Atom, Evaluation, Result, Time}

/**
  * Created by FM on 22.04.16.
  */
object StreamingAspTransformation {
  val now = Atom("now")


  def transform(evaluation: Set[Evaluation]) = {
    evaluation flatMap (x => transformAtoms(x.time, x.atoms))
  }

  def atomAtT(time: Time, atom: Atom) = {
    val timeParameter = time.milliseconds.toString

    Fact(atom(timeParameter))
  }

  def transformAtoms(time: Time, atoms: Set[Atom]) = {
    val atomsWithT = atoms.map(x => atomAtT(time, x))

    atomsWithT map (x => AspConversion(x))
  }

  // TODO: naming of previousEvalutions, and Set[Evaluation]
  def transform(currentTime: Time, previousEvalutions: Set[Evaluation]) = {
    val evalu = transform(previousEvalutions)

    val transformedAtoms = evalu + AspConversion(atomAtT(currentTime, now))

    // TODO: do we need the last clause?
    transformedAtoms ++ (previousEvalutions flatMap (x => x.atoms.map(Fact(_))) map (x => AspConversion(x)))
  }
}

case class StreamingAspTransformation(aspExpressions: Set[AspExpression], aspEngine: Asp = Asp()) extends AspEvaluation {

  def prepare(time: Time, atoms: Set[Atom]): Result = {

    // TOOD: fix
    val e = Evaluation(time, atoms)
    val transformed = StreamingAspTransformation.transform(time, Set(e))

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
