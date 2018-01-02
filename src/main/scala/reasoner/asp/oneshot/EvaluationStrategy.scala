package reasoner.asp.oneshot

import core.asp.NormalProgram

import scala.concurrent.duration._

/**
  * Created by FM on 13.05.16.
  */


trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode
