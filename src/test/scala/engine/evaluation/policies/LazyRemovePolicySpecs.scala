package engine.evaluation.policies

import core.Atom
import core.asp.NormalRule
import core.lars.Duration
import engine.asp.tms.GroundedNormalRule
import engine.asp.tms.policies.LazyRemovePolicy
import fixtures.TimeTestFixtures
import jtms.{Jtms, JtmsGreedy}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 12.06.16.
  */
class LazyRemovePolicySpecs extends FlatSpec with TimeTestFixtures {

  class JTmsSpy extends JtmsGreedy {

    override def allAtoms() = addCalls flatMap(_.atoms) to

    var addCalls = List[NormalRule]()
    var removeCalls = List[NormalRule]()

    def reset = {
      addCalls = List()
      removeCalls = List()
    }

    override def add(rule: NormalRule): Unit = addCalls = addCalls :+ rule

    override def set(model: Set[Atom]): Boolean = false

    override def remove(rule: NormalRule): Unit = removeCalls = removeCalls :+ rule

    override def getModel(): Option[Set[Atom]] = None
  }

  val sr = GroundedNormalRule(a)

  case class LazyPolicy(laziness: Duration = 0) {
    val spy = new JTmsSpy
    val policy = new LazyRemovePolicy(spy, laziness)
  }

  "An empty policy" should "have one addCall after adding" in new LazyPolicy {
    policy.add(t0)(Seq(sr))

    spy.addCalls should have length (1)
  }

  "Remove on a rule" should "not trigger immediately a remove in the TMS" in new LazyPolicy {
    policy.add(t0)(Seq(sr))
    spy.reset

    policy.remove(t1)(Seq(sr))

    spy.removeCalls shouldBe empty
  }

  "Removing and re-adding the same rule" should "not trigger an add in the TMS" in new LazyPolicy {
    policy.add(t0)(Seq(sr))
    policy.remove(t0)(Seq(sr))
    spy.reset

    policy.add(t1)(Seq(sr))

    spy.addCalls shouldBe empty
  }

  "Removing a rule and adding another rule" should "trigger the remove call on the TMS" in new LazyPolicy {
    policy.add(t0)(Seq(sr))
    policy.remove(t0)(Seq(sr))

    assume(spy.removeCalls.isEmpty)
    spy.reset

    policy.add(t1)(Seq(GroundedNormalRule(b)))

    spy.addCalls should have length (1)
    spy.removeCalls should have length (1)
  }

  "Removing a rule and adding another rule with laziness 1" should "not trigger the remove call on the TMS" in new LazyPolicy(1) {
    policy.add(t0)(Seq(sr))
    policy.remove(t0)(Seq(sr))
    spy.reset

    policy.add(t1)(Seq(GroundedNormalRule(b)))

    spy.addCalls should have length (1)
    spy.removeCalls shouldBe empty
  }
  it should "trigger it at t2" in new LazyPolicy(1) {
    policy.add(t0)(Seq(sr))
    policy.remove(t0)(Seq(sr))
    spy.reset

    policy.add(t2)(Seq(GroundedNormalRule(b)))

    spy.addCalls should have length (1)
    spy.removeCalls should have length (1)
  }
}
