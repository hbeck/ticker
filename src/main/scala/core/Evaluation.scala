package core

import core.asp.NormalProgram

/**
  * Created by FM on 25.02.16.
  */
trait Evaluation extends (NormalProgram => Set[Model])

