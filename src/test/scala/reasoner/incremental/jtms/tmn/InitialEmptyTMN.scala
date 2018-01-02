package reasoner.incremental.jtms.tmn

import core._
import core.asp.{AspFact, AspProgram}
import reasoner.incremental.jtms.algorithms.JtmsDoyle
import reasoner.incremental.jtms.in
import org.scalatest.FlatSpec

import scala.language.implicitConversions

/**
  * Created by FM on 05.02.16.
  */
class InitialEmptyTMN extends FlatSpec {

  val assumptionA = AspFact(Atom("A"))
  val program = AspProgram(assumptionA)

  val EmptyTMN = JtmsDoyle(AspProgram())
  val tmn = {
    JtmsDoyle(program)
  }

  val net = tmn.network

  "An empty TMN" should "have no rules and no status values set" in {
    val empty = EmptyTMN
    assert(empty.network.rules.size == 0)
    assert(empty.network.status.size == 0)
  }

  "A TMN with one assumption" should "generate a status entry after update" in {
    assert(net.status.isEmpty == false)
  }

  it should "contain one entry with status in" in {
    assert(net.status.filter(_._2 == in).size == 1)
  }

  it should "contain one rule" in {
    assert(net.rules.size == 1)
  }

  it should "have no consequences and no support" in {
    assert(net.cons(assumptionA.head).size == 0)
    assert(net.supp(assumptionA.head).size == 0)
  }

  "An TMN with the same assumptions applied two times" should "still contain one status" in {
    tmn.add(assumptionA)

    assert(net.status.size == 1)
  }
  it should "also contain just one rule" in {
    tmn.add(assumptionA)

    assert(net.rules.size == 1)
  }
}
