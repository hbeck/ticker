package core.lars

import core.Atom

/**
  * Created by FM on 29.04.16.
  */
case class WindowAtom(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) extends ExtendedAtom


//trait WindowFunction extends ((WindowSize)  (Stream, Time) => Stream)
