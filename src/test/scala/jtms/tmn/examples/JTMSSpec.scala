package jtms.tmn.examples

import core.{Atom, Program, Rule}
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

  val none = Set[Atom]()

  val j1 = Rule(A,C)
  val j2 = Rule(B,none,Set(A))
  val j3 = Rule(C,A)
  val j4a = Rule(D,B)
  val j4b = Rule(D,C)
  val j5 = Rule(E)
  val j6 = Rule(F,Set(C, E))

  val program = Program(j1, j2, j3, j4a, j4b, j5, j6)

  def JTMS = {
    val tmn = TMN(program)

    tmn
  }
}
