package jtms.asp.examples

import core.Atom
import core.asp.{AspFact, AspProgram, AspRule}
import fixtures.AtomTestFixture
import jtms.algorithms.JtmsLearn
import jtms.asp.LimitationHandling.assertModelWithKnownLimitation
import jtms.in
import jtms.networks.OptimizedNetworkForLearn
import jtms.tmn.examples.TweetyBehavior
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
class Deletion extends FlatSpec with AtomTestFixture {

  val none = Set[Atom]()

  "A model with only one rule" should "have no rules and atoms after deletion" in {

    val net = new OptimizedNetworkForLearn()
    val update = new JtmsLearn(net)

    update.add(AspFact(a))

    assume(update.getModel.get == Set(a))
    assume(net.status(a) == in)

    update.remove(AspFact(a))

    assert(net.allAtoms.isEmpty)
    assert(net.status.isEmpty)

    assert(net.rules.isEmpty)
    assert(update.getModel.get.isEmpty)

    assert(net.cons.isEmpty)
    assert(net.supp.isEmpty)
  }

  it should "have clean caches after deletion" in {
    val net = new OptimizedNetworkForLearn()
    val update = new JtmsLearn(net)

    update.add(AspFact(a))

    assume(update.getModel.get == Set(a))
    assume(net.status(a) == in)

    update.remove(AspFact(a))

    net.__cleanupSupportingData(true)
    assert(net.__rulesAtomsOccursIn.isEmpty)
    assert(net.__justifications.isEmpty)
  }


  "Removing the rule 'a :-c' in a program ('a :- c','a :- c, b')" should "still have cons(c) = a " in {
    val r1 = AspRule(a, Set(c))
    val r2 = AspRule(a, Set(c, b))

    val program = AspProgram(r1, r2)

    val net = JtmsLearn(program)

    assume(net.network.cons(c) == Set(a))

    net.remove(r1)

    assert(net.network.cons(c) == Set(a))
  }

  "A stable TMN with 2 atoms and two rules" should "have an empty model after deletion of a supporting Premise" in {
    // arrange
    val r0 = AspRule(b, a)
    val r1 = AspRule(a, none, none)

    val net = new OptimizedNetworkForLearn()
    val update = new JtmsLearn(net)

    update.add(r0)
    update.add(r1)

    assume(update.getModel.get == Set(a, b))

    // act
    update.remove(r1)

    // assert
    assert(update.getModel.get.isEmpty)

    assert(net.rules == Set(r0))
    assert(net.supp(a).isEmpty)
    assert(net.suppRule(a) == None)
    assert(net.suppRule(b) == None)
    assert(net.cons(a) == Set(b))
    assert(net.allAtoms == Set(a, b))
    assert(net.status.keys == Set(a, b))
  }

  it should "have the Model A after deletion of a rule" in {
    // arrange
    val r1 = AspRule(b, a)
    val r2 = AspFact(a)

    val net = new OptimizedNetworkForLearn()
    val update = new JtmsLearn(net)

    update.add(r1)
    update.add(r2)

    assume(update.getModel.get == Set(a, b))

    // act
    update.remove(r1)

    // assert
    assert(update.getModel.get == Set(a))

    assert(net.rules == Set(r2))
    assert(net.supp(a) == Set())
    assert(net.suppRule(a) == Some(r2))
    assert(net.cons(a) == Set())

    assert(net.allAtoms == Set(a))
    assert(net.status.keys == Set(a))
  }

  "A TMN with three atoms" should "have only Model A after deleting a rule" in {
    val r0 = AspRule.pos(a).head(b)
    val r1 = AspFact(a)
    val r2 = AspRule.pos(b).head(c)

    val net = new OptimizedNetworkForLearn()
    val update = new JtmsLearn(net)

    update.add(r0)
    update.add(r1)
    update.add(r2)

    assume(update.getModel.get == Set(a, b, c))

    update.remove(r0)

    assert(update.getModel.get == Set(a))

    assume(net.rules.toSet == Set(r1, r2))
    assert(net.supp(c) == Set(b))
    assert(net.cons(a) == Set())
    assert(net.suppRule(c) == None)

    assert(net.allAtoms == Set(a, b, c))
    assert(net.status.keys == Set(a, b, c))
  }

  "A TMN with three atoms and a redundant rule" should "have Model A,C after deleting a rule supporting B" in {
    val r0 = AspRule.pos(a).head(b)
    val r1 = AspFact(a)
    val r2 = AspRule.pos(b).head(c)
    val r3 = AspRule.pos(a).head(c)

    val net = new OptimizedNetworkForLearn()
    val update = new JtmsLearn(net)

    update.doForceChoiceOrder = true
    update.choiceSeq = Seq(b, a)

    update.add(r0)
    update.add(r1)
    update.add(r2)

    // do to an optimization these assertions are only valid before a new rule is added
    assume(update.getModel.get == Set(a, b, c))
    assume(net.suppRule(c) == Some(r2))
    assume(net.supp(c) == Set(b))
    assume(net.cons(a) == Set(b))
    assume(net.cons(b) == Set(c))

    update.add(r3)

    update.remove(r0)

    assert(update.getModel.get == Set(a, c))
    assert(net.rules.toSet == Set(r1, r2, r3), "the SJ for C should change")
    info("the SJ for C should change")
    assert(net.suppRule(c) == Some(r3))
    info("the supp for C should change")
    assert(net.supp(c) == Set(a))
    info("the cons for A should change")
    assert(net.cons(a) == Set(c))
    assert(net.cons(b) == Set(c))
  }

  "Removing an additional rule form the JTMS 5 sample" should "result in the original model" in {
    // arrange
    val setup = new Jtms_5_Asp
    val tms = setup.net

    assert(tms.getModel.get == Set(setup.a, setup.c, setup.d, setup.e, setup.f))

    // act
    tms.remove(setup.j0)

    // assert
    // assert(net.getModel.get == Set(setup.e, setup.b, setup.d))
    tms match {
      case x: JtmsLearn => assertModelWithKnownLimitation(x, Set(e, b, d), x.choiceSeq.head == d)
      case _ => assertModelWithKnownLimitation(tms, Set(e, b, d),
        tms.choiceSeq.head == d || (tms.choiceSeq(0) == c && tms.choiceSeq(1) == d)
      )
    }

  }

  "Removing the Penguin premise from the Tweety sample" should "result in the Model V, F" in {
    // arrange
    class Tweety extends FlatSpec with TweetyBehavior

    val setup = new Tweety

    val net = JtmsLearn(setup.program)

    net.add(setup.j5)

    assume(net.getModel.get == Set(setup.F_not, setup.V, setup.P))

    // act
    net.remove(setup.j5)

    // assert
    assert(net.getModel.get == Set(setup.V, setup.F))
  }

}
