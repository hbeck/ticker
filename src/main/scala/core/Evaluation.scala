package core

import core.asp.AspProgram

/**
  * Created by FM on 25.02.16.
  */
trait Evaluation {
  // TODO: needs to be Program
  def apply(program: AspProgram): Set[Model]
}


