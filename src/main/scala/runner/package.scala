import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.11.16.
  */
package object runner {
  type Startable = () => Unit

  sealed trait OutputEvery

  object Diff extends OutputEvery

  case class Time(interval: Option[Duration] = None) extends OutputEvery

  case class Signal(interval: Int = 1) extends OutputEvery
}
