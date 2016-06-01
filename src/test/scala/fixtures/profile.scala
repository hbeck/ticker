package fixtures

import scala.concurrent.duration._
import scala.language.{postfixOps, implicitConversions}


/**
  * Created by FM on 28.05.16.
  */
package object profile {

  def withWarmup[R](code: => R): R = withWarmup(1)(code)

  def withWarmup[R](repeat: Int)(code: => R): R = {
    (1 until Math.max(repeat / 10, 1)).foldLeft(code){ (_: R, _: Int) => code }

    profileR(repeat)(code)
  }

  def profile[R](code: => R): R = profileR(1)(code)

  def profileR[R](repeat: Int)(code: => R): R = {
    require(repeat > 0, "Profile: at least 1 repetition required")

    val start = Deadline.now

    val result = (1 until repeat).foldLeft(code) { (_: R, _: Int) => code }

    val end = Deadline.now

    val elapsed = ((end - start) / repeat)

    if (repeat > 1) {
      println(s"Elapsed time: $elapsed averaged over $repeat repetitions; Total elapsed time")

      val totalElapsed = (end - start)

      println(s"Total elapsed time: $totalElapsed")
    }
    else println(s"Elapsed time: $elapsed")

    result
  }
}
