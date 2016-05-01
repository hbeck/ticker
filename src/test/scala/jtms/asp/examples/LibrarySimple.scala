package jtms.asp.examples

import core._
import core.asp.{AspFact, AspProgram, AspRule}
import jtms.ExtendedJTMS
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class LibrarySimple extends FunSuite {

  val V = Atom("verfuegbar")
  val G = Atom("gestohlen")
  val P = Atom("am_angegebenen_Platz_vorhanden")
  val F = Atom("falsch_einsortiert")
  val P_not = Atom("nicht_am_angegebenen_Platz_vorhanden")
  val A = Atom("ausleihbar")
  val N = Atom("nachschlagewerk")
  val A_not = Atom("nicht_ausleihbar")
  val H = Atom("im_Handapperart_einer_Veranstaltung")

  val Falsum = new ContradictionAtom("f")

  val j1 = AspFact(V)
  val j2 = AspRule.pos(V).neg(F, G).head(P)
  val j3 = AspRule.pos(F).head(P_not)
  val j4 = AspRule.pos(G).head(P_not)
  val j5 = AspRule.pos(P).neg(H, N).head(A)
  val j6 = AspRule.pos(P, P_not).head(Falsum)
  val j7 = AspRule.pos(N).head(A_not)
  val j8 = AspRule.pos(H).head(A_not)
  val j9 = AspRule.pos(A, A_not).head(Falsum)

  val program = AspProgram(j1, j2, j3, j4, j5, j6, j7, j8, j9)

  test("1") {
    assert(ExtendedJTMS(program).getModel.get == Set(V, P, A))
  }

  test("2") {
    assert(ExtendedJTMS(program + AspFact(H)).getModel.get == Set(V, P, A_not, H))
  }

  test("3") {
    assert(ExtendedJTMS(program + AspRule(Falsum,Set(A))).getModel == None)
  }

  test("4") {
    assert(ExtendedJTMS(program + AspRule(Falsum,Set(P),Set[Atom]())).getModel == None)
  }

}
