package evaluation.diss

import core._
import evaluation.diss.Helpers.string2Atom

/**
  * Created by hb on 05.04.18.
  */
object Prepared {

  val T = Variable("T")
  val U = Variable("U")
  val V = Variable("V")
  val W = Variable("W")
  val X = Variable("X")
  val Y = Variable("Y")
  val Z = Variable("Z")

  val a: Atom = "a"
  val b: Atom = "b"

  val gX: Atom = "g(X)"
  val gY: Atom = "g(Y)"
  val gZ: Atom = "g(Z)"

  def b(i: Int, j: Int): Atom = "b("+i+","+j+")"
  def g(i: Int): Atom = "g("+i+")"



}
