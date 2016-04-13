package engine

/**
  * Created by FM on 05.04.16.
  */
case class Time(milliseconds: Long) {

}

object Seconds {
  def apply(seconds: Long) = Time(seconds * 1000)
}

object Minute {
  def apply(minutes: Long) = Seconds(minutes * 60)
}
