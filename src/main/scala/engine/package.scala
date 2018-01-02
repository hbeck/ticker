import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.11.16.
  */
package object engine {
  type Startable = () => Unit

  sealed trait OutputTiming //parameter --outputEvery

  object Change extends OutputTiming

  case class Time(interval: Option[Duration] = None) extends OutputTiming

  case class Signal(interval: Int = 1) extends OutputTiming
}
