package jtms.tmn

import core.{Program, Rule, Fact, Atom}
import jtms.tmn.examples._
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

    val tmn = new TMN(Set(A))
    tmn.add(j0)

    assume(tmn.getModel.get == Set(A))
    assume(tmn.status(A) == in)

    tmn.remove(j0)

    assert(tmn.N.isEmpty)
    assert(tmn.status.isEmpty)

    assert(tmn.rules.isEmpty)
    assert(tmn.getModel == None)

    assert(tmn.ConsRules.isEmpty)
    assert(tmn.Supp.isEmpty)
    assert(tmn.SuppRule.isEmpty)
  }

  "Removing the rule 'a :-c' in a program ('a :- c','a :- c, b')" should "still have Cons(c) = a " in {
    val r1 = Rule.pos(C).head(A)
    val r2 = Rule.pos(C, B).head(A)

    val program = Program(r1, r2)

    val tmn = TMN(program)

    assume(tmn.Cons(C) == Set(A))

    tmn.remove(r1)

    assert(tmn.Cons(C) == Set(A))
  }

  "A stable TMN with 2 atoms and two rules" should "have an empty model after deletion of a supporting Premise" in {
    // arrange
    val j0 = Rule.pos(A).head(B)
    val j1 = Fact(A)

    val tmn = new TMN(Set(A, B))

    tmn.add(j0)
    tmn.add(j1)

    assume(tmn.getModel.get == Set(A, B))

    // act
    tmn.remove(j1)

    // assert
    assert(tmn.getModel.isEmpty)

    assert(tmn.rules == List(j0))
    assert(tmn.Supp(A).isEmpty)
    assert(tmn.SuppRule(A) == None)
    assert(tmn.SuppRule(B) == None)
    assert(tmn.Cons(A) == Set(B))
    assert(tmn.N == Set(A, B))
    assert(tmn.status.keys == Set(A, B))
  }

  it should "have the Model A after deletion of a rule" in {
    // arrange
    val j0 = Rule.pos(A).head(B)
    val j1 = Fact(A)

    val tmn = new TMN(Set(A, B))

    tmn.add(j0)
    tmn.add(j1)

    assume(tmn.getModel.get == Set(A, B))

    // act
    tmn.remove(j0)

    // assert
    assert(tmn.getModel.get == Set(A))

    assert(tmn.rules == List(j1))
    assert(tmn.Supp(A) == Set())
    assert(tmn.SuppRule(A) == Some(j1))
    assert(tmn.Cons(A) == Set())

    assert(tmn.N == Set(A))
    assert(tmn.status.keys == Set(A))
  }

  "A TMN with three atoms" should "have only Model A after deleting a rule" in {
    val j0 = Rule.pos(A).head(B)
    val j1 = Fact(A)
    val j2 = Rule.pos(B).head(C)

    val tmn = new TMN(Set(A, B, C))

    tmn.add(j0)
    tmn.add(j1)
    tmn.add(j2)

    assume(tmn.getModel.get == Set(A, B, C))

    tmn.remove(j0)

    assert(tmn.getModel.get == Set(A))

    assume(tmn.rules.toSet == Set(j1, j2))
    assert(tmn.Supp(C) == Set(B))
    assert(tmn.Cons(A) == Set())
    assert(tmn.SuppRule(C) == None)

    assert(tmn.N == Set(A, B, C))
    assert(tmn.status.keys == Set(A, B, C))
  }

  "A TMN with three atoms and a redundant rule" should "have Model A,C after deleting a rule supporting B" in {
    val j0 = Rule.pos(A).head(B)
    val j1 = Fact(A)
    val j2 = Rule.pos(B).head(C)
    val j3 = Rule.pos(A).head(C)

    val tmn = new TMN(Set(A, B, C))

    tmn.add(j0)
    tmn.add(j1)
    tmn.add(j2)
    tmn.add(j3)

    assume(tmn.getModel.get == Set(A, B, C))
    assume(tmn.SuppRule(C) == Some(j2))
    assume(tmn.Supp(C) == Set(B))
    assume(tmn.Cons(A) == Set(B, C))
    assume(tmn.Cons(B) == Set(C))

    tmn.remove(j0)

    assert(tmn.getModel.get == Set(A, C))
    assert(tmn.rules.toSet == Set(j1, j2, j3), "the SJ for C should change")
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

    assume(tmn.getModel.get == Set(setup.a, setup.c, setup.d, setup.e, setup.f))

    // act
    tmn.remove(setup.j0)

    // assert
    assert(tmn.getModel.get == Set(setup.e, setup.b, setup.d))
  }

  "Removing the Penguin premise from the Tweety sample" should "result in the Model V, F" in {
    // arrange
    class Tweety extends FlatSpec with TweetyBehavior

    val setup = new Tweety

    val tmn = TMN(setup.program)

    tmn.add(setup.j5)

    assume(tmn.getModel.get == Set(setup.F_not, setup.V, setup.P))

    // act
    tmn.remove(setup.j5)

    // assert
    assert(tmn.getModel.get == Set(setup.V, setup.F))
  }

  "Removing a rule from a TMN where backtracking occurred" should "result in the original model" in {
    // arrange
    class JMTS_21 extends JTMSSpec with JTMS_21Behavior
    val setup = new JMTS_21
    val tmn = TMN(setup.p)

    assume(tmn.getModel.get == Set(setup.a, setup.c, setup.d, setup.f, setup.e))

    // act
    tmn.remove(setup.j7)

    // assert
    assert(tmn.getModel == setup.JTMS.getModel())
    assert(tmn.getModel.get == Set(setup.e, setup.b, setup.d))
  }

  "Removing a exclusion rule for A in the library sample" should "result in the initial model" in {
    val setup = new LibraryAtomValidation
    val tmn = setup.Tmn

    tmn.add(setup.jExclusionA)

    assume(tmn.getModel.get == Set(setup.A_not, setup.H, setup.P, setup.V))

    // act
    tmn.remove(setup.jExclusionA)

    // assert
    assert(tmn.getModel.get == Set(setup.A, setup.P, setup.V))
  }
}
