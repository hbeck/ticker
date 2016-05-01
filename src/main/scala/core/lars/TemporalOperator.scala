package core.lars

import core.Atom
import engine.{Time, _}

/**
  * Created by FM on 01.05.16.
  */
sealed trait TemporalOperator

object Diamond extends TemporalOperator

object Box extends TemporalOperator

case class At(time: Time) extends TemporalOperator