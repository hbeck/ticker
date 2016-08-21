package core

/**
  * Created by FM on 29.04.16.
  */
package object lars {
  type TimeVariable = Variable
  type WindowSize = Long
  type Duration = Long

  val T = TimeVariableWithOffset(Variable("T"))

  def STW(windowSize: WindowSize) = SlidingTimeWindow(windowSize)

  def W(windowSize: WindowSize, temporalModality: TemporalModality, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), temporalModality, atom)

  type LarsRule = Rule[HeadAtom, ExtendedAtom]

}
