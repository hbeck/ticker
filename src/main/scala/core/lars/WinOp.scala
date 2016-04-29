package core.lars

import core.Atom
import engine.Time

/**
  * Created by FM on 29.04.16.
  */
case class WinOp(windowFunction: WindowFunction, temporalOperator: TemporalOperator, atom: Atom) extends Atom/* atom? */ {

}


//trait WindowFunction extends ((WindowSize)  (Stream, Time) => Stream)
