package jtms.tmn.examples

import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMS extends FlatSpec {

  val A = Node("A")
  val B = Node("B")
  val C = Node("C")
  val D = Node("D")
  val E = Node("E")
  val F = Node("F")

  val j1 = Justification.in(C).node(A)
  val j2 = Justification.out(A).node(B)
  val j3 = Justification.in(A).node(C)
  val j4a = Justification.in(B).node(D)
  val j4b = Justification.in(C).node(D)
  val j5 = Premise(E)
  val j6 = Justification.in(C, E).node(F)

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
