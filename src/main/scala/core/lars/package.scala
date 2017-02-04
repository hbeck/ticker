package core

/**
  * Created by FM on 29.04.16.
  */
package object lars {

  type TimeVariable = Variable
  //  type WindowSize = (Long, TimeUnit)
  type TupleCount = Long
  type Duration = Long

  type EngineTimeUnit = scala.concurrent.duration.Duration //temporal duration of a logical time point
  type TimeUnit = scala.concurrent.duration.TimeUnit

  //TODO hb move to engine asp package (like now/cnt/pin)
  val T = TimeVariableWithOffset(Variable("T"))
  val C = Variable("C")
  val D = Variable("D")

  val U = Variable("U")

  def TimeW(windowSize: TimeWindowSize) = SlidingTimeWindow(windowSize)

  //TODO hb TupleW

  def W(windowSize: TimeWindowSize, temporalModality: TemporalModality, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), temporalModality, atom)

  def Fluent(atom: Atom) = WindowAtom(FluentWindow, Diamond, atom)

  type LarsRule = Rule[HeadAtom, ExtendedAtom]

}
