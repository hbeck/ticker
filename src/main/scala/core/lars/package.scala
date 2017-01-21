package core

/**
  * Created by FM on 29.04.16.
  */
package object lars {
  type TimeVariable = Variable
  //  type WindowSize = (Long, TimeUnit)
  type TupleCount = Long
  type Duration = Long

  type EngineTick = scala.concurrent.duration.Duration
  type TimeUnit = scala.concurrent.duration.TimeUnit

  val T = TimeVariableWithOffset(Variable("T"))
  val D = Variable("D")

  def STW(windowSize: TimeWindowSize) = SlidingTimeWindow(windowSize)

  def W(windowSize: TimeWindowSize, temporalModality: TemporalModality, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), temporalModality, atom)

  def Fluent(atom: Atom) = WindowAtom(FluentWindow, Diamond, atom)

  type LarsRule = Rule[HeadAtom, ExtendedAtom]

}
