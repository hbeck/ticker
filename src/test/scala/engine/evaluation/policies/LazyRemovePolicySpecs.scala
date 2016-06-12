package engine.evaluation.policies

import core.Atom
import core.asp.NormalRule
import engine.asp.evaluation.GroundedNormalRule
import engine.asp.evaluation.policies.LazyRemovePolicy
import fixtures.TimeTestFixtures
import jtms.Jtms
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 12.06.16.
  */
class LazyRemovePolicySpecs extends FlatSpec with TimeTestFixtures {

  class JTmsSpy extends Jtms {

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

  trait PartialPolicy {
    val spy = new JTmsSpy
    val policy = new LazyRemovePolicy(spy)
  }

  "An empty policy" should "have one addCall after adding" in new PartialPolicy {
    policy.add(t0)(Seq(sr))

    spy.addCalls should have length (1)
  }

  "Remove on a rule" should "not trigger immediately a remove in the TMS" in new PartialPolicy {
    policy.add(t0)(Seq(sr))
    spy.reset

    policy.remove(t1)(Seq(sr))

    spy.removeCalls shouldBe empty
  }

  "Removing and re-adding the same rule" should "not trigger an add in the TMS" in new PartialPolicy {
    policy.add(t0)(Seq(sr))
    policy.remove(t0)(Seq(sr))
    spy.reset

    policy.add(t1)(Seq(sr))

    spy.addCalls shouldBe empty
  }

  "Removing a rule and adding another rule" should "trigger the remove call on the TMS" in new PartialPolicy {
    policy.add(t0)(Seq(sr))
    policy.remove(t0)(Seq(sr))
    spy.reset

    policy.add(t1)(Seq(GroundedNormalRule(b)))

    spy.addCalls should have length(1)
    spy.removeCalls should have length(1)
  }
}
