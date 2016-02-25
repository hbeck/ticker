package core

import core.EvaluationResult.Model

/**
  * Created by FM on 25.02.16.
  */
trait Evaluation {
  def apply(program: Program): Option[EvaluationResult]
}

object EvaluationResult {
  type Model = Set[Atom]
}

abstract class EvaluationResult {
  def contains(atoms: Set[Atom]): Boolean

  def contains(atoms: Atom*) : Boolean= contains(atoms.toSet)
}

case class SingleModel(model: Model) extends EvaluationResult {
  override def contains(atoms: Set[Atom]) = model.intersect(atoms) == atoms
}

case class MultipleModels(models: Set[Model]) extends EvaluationResult {
  override def contains(atoms: Set[Atom]) = models.exists(model =>  model.intersect(atoms) == atoms)
}
