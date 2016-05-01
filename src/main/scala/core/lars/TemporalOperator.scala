package core.lars

import engine.Time

/**
  * Created by FM on 01.05.16.
  */
sealed trait TemporalOperator

object Diamond extends TemporalOperator

object Box extends TemporalOperator

case class At(time: Time) extends TemporalOperator
