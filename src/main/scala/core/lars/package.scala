package core

/**
  * Created by FM on 29.04.16.
  */
package object lars {
  type WindowSize = Long
  type Duration = Long

  val T = TimeVariable("T")

  def STW(windowSize: WindowSize) = SlidingTimeWindow(windowSize)

  def W(windowSize: WindowSize, temporalModality: TemporalModality, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), temporalModality, atom)

}
