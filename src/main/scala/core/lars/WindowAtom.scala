package core.lars

import core.{Atom, Formula}
import engine.Time

/**
  * Created by FM on 29.04.16.
  */
case class WindowAtom(windowFunction: WindowFunction, temporalOperator: TemporalOperator, atom: Atom) extends Formula


//trait WindowFunction extends ((WindowSize)  (Stream, Time) => Stream)
