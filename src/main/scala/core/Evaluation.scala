package core

/**
  * Created by FM on 25.02.16.
  */
trait Evaluation {
  def apply(program: AspProgram): Set[Model]
}


