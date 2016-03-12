package jtms.tmn.consistency

import core.{Atom, ContradictionAtom, Rule}
import jtms.TMN
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class Consistency extends FunSuite {

  val A = Atom("A")
//  val B = Atom("B")
//  val C = Atom("C")
//  val D = Atom("D")
//  val E = Atom("E")
//  val F = Atom("F")

  val n = ContradictionAtom("n")

  val none = Set[Atom]()

  //base case for next (no ddb here)
  test("a") {
    val tmn = TMN()
    tmn.add(Rule(A))
    val model = tmn.model.get
    assert(model.size == 1)
    assert(model contains A)
  }

  //inconsistent
  test(":- not a") {
    val tmn = TMN()
    tmn.add(Rule(n,none,Set(A)))
    assert(tmn.model == None)
  }

  //inconsistent
  test("a. :- a.") {
    val tmn = TMN()
    tmn.add(Rule(A))
    tmn.add(Rule(n,A))
    assert(tmn.model == None)
  }

}
