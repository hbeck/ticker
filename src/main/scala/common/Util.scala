package common

/**
  * Created by hb on 8/28/16.
  */
object Util {

  def printTime[T](nameOfProcedure: String)(any: => T): T = {
    val start = System.currentTimeMillis()
    val result: T = any
    val end = System.currentTimeMillis()
    println(nameOfProcedure + ": " + (1.0 * (end - start)) / 1000.0 + " sec")
    result
  }

  def stopTime[T](any: => T): Long = {
    val start = System.currentTimeMillis()
    val result: T = any
    val end = System.currentTimeMillis()
    end - start
  }

  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _: java.lang.NumberFormatException => None
  }

}

