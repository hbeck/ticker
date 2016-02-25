package core

import core.Evaluation.Model

/**
  * Created by FM on 25.02.16.
  */
object Evaluation{
  type Model = Set[Atom]
}

trait Evaluation {
  def apply(program: Program): Set[Model]
}