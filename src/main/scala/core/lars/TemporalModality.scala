package core.lars

import engine.Time

/**
  * Created by FM on 01.05.16.
  */
sealed trait TemporalModality

case object Diamond extends TemporalModality

case object Box extends TemporalModality

case class At(time: Time) extends TemporalModality
