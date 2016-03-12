package jtms.tmn

import core.{Rule, Fact, Atom}
import jtms.tmn.examples.{Library, Tweety, JTMS_5, JTMS_21}
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
class Deletion extends FlatSpec {

  val A = Atom("A")
  val B = Atom("B")
  val C = Atom("C")

  "A model with only one rule" should "have no rules and atoms after deletion" in {
    val j0 = Fact(A)

    val tmn = TMN()//Set(A))
    tmn.add(j0)

    assume(tmn.model().get == Set(A))
    assume(tmn.status(A) == in)

    tmn.remove(j0)

    assert(tmn.atoms.isEmpty)
    assert(tmn.status.isEmpty)

    assert(tmn.rules.isEmpty)
    assert(tmn.model().get == Set())

    assert(tmn.Cons.isEmpty)
    assert(tmn.Supp.isEmpty)
    assert(tmn.SuppRule.isEmpty)
  }

  "A stable TMN with 2 atoms and two rules" should "have an empty model after deletion of a supporting Premise" in {
    // arrange
    val j0 = Rule.pos(A).head(B)
    val j1 = Fact(A)

    val tmn = TMN() //Set(A, B))

    tmn.add(j0)
    tmn.add(j1)

    assume(tmn.model().get == Set(A, B))

    // act
    tmn.remove(j1)

    // assert
    assert(tmn.model().get.isEmpty)

    assert(tmn.rules == Set(j0))
    assert(tmn.Supp(A).isEmpty)
    assert(tmn.SuppRule(A) == None)
    assert(tmn.SuppRule(B) == None)
    assert(tmn.Cons(A) == Set(B))
    assert(tmn.atoms == Set(A, B))
    assert(tmn.status.keys == Set(A, B))
  }

  it should "have the Model A after deletion of a rule" in {
    // arrange
    val j0 = Rule.pos(A).head(B)
    val j1 = Fact(A)

    val tmn = TMN() //Set(A, B))

    tmn.add(j0)
    tmn.add(j1)

    assume(tmn.model().get == Set(A, B))

    // act
    tmn.remove(j0)

    // assert
    assert(tmn.model().get == Set(A))

    assert(tmn.rules == Set(j1))
    assert(tmn.Supp(A) == Set())
    assert(tmn.SuppRule(A) == Some(j1))
    assert(tmn.Cons(A) == Set())

    assert(tmn.atoms == Set(A))
    assert(tmn.status.keys == Set(A))
  }

  "A TMN with three atoms" should "have only Model A after deleting a rule" in {
    val j0 = Rule.pos(A).head(B)
    val j1 = Fact(A)
    val j2 = Rule.pos(B).head(C)

    val tmn = TMN() //Set(A, B, C))

    tmn.add(j0)
    tmn.add(j1)
    tmn.add(j2)

    assume(tmn.model().get == Set(A, B, C))

    tmn.remove(j0)

    assert(tmn.model().get == Set(A))

    assume(tmn.rules == Set(j1, j2))
    assert(tmn.Supp(C) == Set(B))
    assert(tmn.Cons(A) == Set())
    assert(tmn.SuppRule(C) == None)

    assert(tmn.atoms == Set(A, B, C))
    assert(tmn.status.keys == Set(A, B, C))
  }

  "A TMN with three atoms and a redundant rule" should "have Model A,C after deleting a rule supporting B" in {
    val j0 = Rule.pos(A).head(B)
    val j1 = Fact(A)
    val j2 = Rule.pos(B).head(C)
    val j3 = Rule.pos(A).head(C)

    val tmn = TMN() //Set(A, B, C))

    tmn.add(j0)
    tmn.add(j1)
    tmn.add(j2)
    tmn.add(j3)

    assume(tmn.model().get == Set(A, B, C))
    assume(tmn.SuppRule(C) == Some(j2))
    assume(tmn.Supp(C) == Set(B))
    assume(tmn.Cons(A) == Set(B, C))
    assume(tmn.Cons(B) == Set(C))

    tmn.remove(j0)

    assert(tmn.model().get == Set(A, C))
    assert(tmn.rules == Set(j1, j2, j3), "the SJ for C should change")
    info("the SJ for C should change")
    assert(tmn.SuppRule(C) == Some(j3))
    info("the Supp for C should change")
    assert(tmn.Supp(C) == Set(A))
    info("the Cons for A should change")
    assert(tmn.Cons(A) == Set(C))
    assert(tmn.Cons(B) == Set(C))
  }

  "Removing an additional rule form the JTMS 5 sample" should "result in the original model" in {
    // arrange
    val setup = new JTMS_5
    val tmn = setup.tmn

    assume(tmn.model().get == Set(setup.A, setup.C, setup.D, setup.E, setup.F))

    // act
    tmn.remove(setup.j0)

    // assert
    assert(tmn.model().get == Set(setup.E, setup.B, setup.D))
  }

  "Removing the Penguin premise from the Tweety sample" should "result in the Model V, F" in {
    // arrange
    val setup = new Tweety
    val tmn = setup.TweetyTMN

    tmn.add(setup.j5)

    assume(tmn.model().get == Set(setup.F_not, setup.V, setup.P))

    // act
    tmn.remove(setup.j5)

    // assert
    assert(tmn.model().get == Set(setup.V, setup.F))
  }

  "Removing a rule from a TMN where backtracking occurred" should "result in the original model" in {
    // arrange
    val setup = new JTMS_21
    val tmn = setup.JTMS_DDB

    assume(tmn.model().get == Set(setup.A, setup.C, setup.D, setup.F, setup.E))

    // act
    tmn.remove(setup.j7)

    // assert
    assert(tmn.model().get == setup.JTMS.model().get)
    assert(tmn.model().get == Set(setup.E, setup.B, setup.D))
  }

  "Removing a exclusion rule for A in the library sample" should "result in the initial model" in {
    val setup = new Library
    val tmn = setup.Tmn

    tmn.add(setup.jExclusionA)

    assume(tmn.model().get == Set(setup.A_not, setup.H, setup.P, setup.V))

    // act
    tmn.remove(setup.jExclusionA)

    // assert
    assert(tmn.model().get == Set(setup.A, setup.P, setup.V))
  }
}
