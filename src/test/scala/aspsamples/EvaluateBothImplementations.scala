package aspsamples

import asp.Asp
import core.Evaluation
import jtms.jTmn
import org.scalatest.FlatSpec

/**
  * Created by FM on 25.02.16.
  */
trait EvaluateBothImplementations {
  this: FlatSpec =>

  val asp = Asp()
  val tmn = new jTmn

  def theSame(tests: => Evaluation => Unit) = {
    "The ASP implementation" should behave like tests(asp)
    "The TMN implementation" should behave like tests(tmn)
  }
}
