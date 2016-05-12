package core

import engine.TimePoint$

/**
  * Created by FM on 29.04.16.
  */
package object lars {
  type WindowSize = Long

  val T = "T"

  def STW(windowSize: WindowSize) = SlidingTimeWindow(windowSize)

  def W(windowSize: WindowSize, temporalModality: TemporalModality, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), temporalModality, atom)

}
