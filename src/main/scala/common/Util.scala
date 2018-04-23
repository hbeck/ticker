package common

import java.util.concurrent.TimeUnit


/**
  * Created by hb on 8/28/16.
  */
object Util {

  //introduced to avoid xml-configuration stuff
  // WARN/ERROR not restricted
  val LOG_LEVEL_DEBUG = 2
  val LOG_LEVEL_INFO = 1
  val LOG_LEVEL_NONE = 0
  var log_level = LOG_LEVEL_NONE //edited by config in main program

  def timeUnitWritten(timeUnit: TimeUnit) = {
    timeUnit match {
      case java.util.concurrent.TimeUnit.MILLISECONDS => "ms"
      case java.util.concurrent.TimeUnit.SECONDS => "s"
      case java.util.concurrent.TimeUnit.MINUTES => "min"
      case java.util.concurrent.TimeUnit.HOURS => "h"
      case _ => "?" //not used
    }
  }

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

  def timeString(millis: Long) = ((1.0 * millis) / 1000.0 + " sec")
  def timeString(millis: Double) = (millis / 1000.0 + " sec")

  /**
    * Pretty prints a Scala value similar to its source represention.
    * Particularly useful for case classes.
    * @param a - The value to pretty print.
    * @param indentSize - Number of spaces for each indent.
    * @param maxElementWidth - Largest element size before wrapping.
    * @param depth - Initial depth to pretty print indents.
    * @return
    */
  def prettyPrint(a: Any, indentSize: Int = 2, maxElementWidth: Int = 30, depth: Int = 0): String = {
    val indent = " " * depth * indentSize
    val fieldIndent = indent + (" " * indentSize)
    val thisDepth = prettyPrint(_: Any, indentSize, maxElementWidth, depth)
    val nextDepth = prettyPrint(_: Any, indentSize, maxElementWidth, depth + 1)
    a match {
      // Make Strings look similar to their literal form.
      case s: String =>
        val replaceMap = Seq(
          "\n" -> "\\n",
          "\r" -> "\\r",
          "\t" -> "\\t",
          "\"" -> "\\\""
        )
        '"' + replaceMap.foldLeft(s) { case (acc, (c, r)) => acc.replace(c, r) } + '"'
      // For an empty Seq just use its normal String representation.
      case xs: Seq[_] if xs.isEmpty => xs.toString()
      case xs: Seq[_] =>
        // If the Seq is not too long, pretty print on one line.
        val resultOneLine = xs.map(nextDepth).toString()
        if (resultOneLine.length <= maxElementWidth) return resultOneLine
        // Otherwise, build it with newlines and proper field indents.
        val result = xs.map(x => s"\n$fieldIndent${nextDepth(x)}").toString()
        result.substring(0, result.length - 1) + "\n" + indent + ")"
      // Product should cover case classes.
      case p: Product =>
        val prefix = p.productPrefix
        // We'll use reflection to get the constructor arg names and values.
        val cls = p.getClass
        val fields = cls.getDeclaredFields.filterNot(_.isSynthetic).map(_.getName)
        val values = p.productIterator.toSeq
        // If we weren't able to match up fields/values, fall back to toString.
        if (fields.length != values.length) return p.toString
        fields.zip(values).toList match {
          // If there are no fields, just use the normal String representation.
          case Nil => p.toString
          // If there is just one field, let's just print it as a wrapper.
          case (_, value) :: Nil => s"$prefix(${thisDepth(value)})"
          // If there is more than one field, build up the field names and values.
          case kvps =>
            val prettyFields = kvps.map { case (k, v) => s"$fieldIndent$k = ${nextDepth(v)}" }
            // If the result is not too long, pretty print on one line.
            val resultOneLine = s"$prefix(${prettyFields.mkString(", ")})"
            if (resultOneLine.length <= maxElementWidth) return resultOneLine
            // Otherwise, build it with newlines and proper field indents.
            s"$prefix(\n${prettyFields.mkString(",\n")}\n$indent)"
        }
      // If we haven't specialized this type, just use its toString.
      case _ => a.toString
    }
  }

}

