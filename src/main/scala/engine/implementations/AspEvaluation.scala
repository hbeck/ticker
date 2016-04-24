package engine.implementations

import asp.AspConversion
import core.Program

/**
  * Created by FM on 24.04.16.
  */
object AspEvaluation {
  def pull(program: Program) = {
    AspPullEvaluation(StreamingAspTransformation(AspConversion(program))
    )
  }
}
