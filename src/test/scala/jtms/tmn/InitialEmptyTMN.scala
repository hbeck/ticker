package jtms.tmn

import core._
import jtms.{JTMNRefactored, in}
import org.scalatest.{FlatSpec, BeforeAndAfter}
import scala.language.implicitConversions

/**
  * Created by FM on 05.02.16.
  */
class InitialEmptyTMN extends FlatSpec {

  val assumptionA = Fact(Atom("A"))
  val program = Program(assumptionA)

  val EmptyTMN = JTMNRefactored(Program())

  val tmn = {
    JTMNRefactored(program)
  }

  "An empty TMN" should "have no rules and no status values set" in {
    val empty = EmptyTMN
    assert(empty.rules.size == 0)
    assert(empty.status.size == 0)
  }

  "A TMN with one assumption" should "generate a status entry after update" in {
    assert(tmn.status.isEmpty == false)
  }

  it should "contain one entry with status in" in {
    assert(tmn.status.filter(_._2 == in).size == 1)
  }

  it should "contain one rule" in {
    assert(tmn.rules.size == 1)
  }

  it should "have no consequences and no support" in {
    assert(tmn.Cons(assumptionA.head).size == 0)
    assert(tmn.Supp(assumptionA.head).size == 0)
  }

  "An TMN with the same assumptions applied two times" should "still contain one status" in {
    tmn.add(assumptionA)

    assert(tmn.status.size == 1)
  }
  it should "also contain just one rule" in {
    tmn.add(assumptionA)

    assert(tmn.rules.size == 1)
  }
}
