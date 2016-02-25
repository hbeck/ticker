package jtms.tmn.examples

import core.{Program, Rule, Premise, Atom}
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMSSpec extends FlatSpec {

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

  val program = Program(j1, j2, j3, j4a, j4b, j5, j6)

  def JTMS = {
    val tmn = TMN(program)

    tmn
  }
}
