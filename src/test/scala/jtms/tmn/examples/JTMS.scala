package jtms.tmn.examples

import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMS extends FlatSpec {

  val A = Atom("A")
  val B = Atom("B")
  val C = Atom("C")
  val D = Atom("D")
  val E = Atom("E")
  val F = Atom("F")

  val j1 = Rule.in(C).head(A)
  val j2 = Rule.out(A).head(B)
  val j3 = Rule.in(A).head(C)
  val j4a = Rule.in(B).head(D)
  val j4b = Rule.in(C).head(D)
  val j5 = Premise(E)
  val j6 = Rule.in(C, E).head(F)

  def JTMS = {
    //    var tmn = new TMN(Set(A, B, C, D, E, F), Set(j1, j2, j3, j4a, j4b, j5, j6).to)

    val tmn = new TMN(Set(A, B, C, D, E, F))
    tmn.add(j1)
    tmn.add(j2)
    tmn.add(j3)
    tmn.add(j4a)
    tmn.add(j4b)
    tmn.add(j5)
    tmn.add(j6)

    tmn
  }
}
