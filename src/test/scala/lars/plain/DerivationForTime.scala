package lars.plain

import core.PinnedAtom
import core.lars._
import engine.asp.PlainLarsToAspMapper
import lars.transform.TransformLarsSpec

/**
  * Created by fm on 21/01/2017.
  */
class DerivationForTime extends TransformLarsSpec {

  val convert = PlainLarsToAspMapper()

  val atWindow = WindowAtom(SlidingTimeWindow(2), At(U), a)

  def convertedRule(windowAtom: WindowAtom) = {
    convert.derivation_(windowAtom).get
  }

  "A w^2 @_U a" should "generate one rule" in {
    val result = convert.derivation_(atWindow)

    assert(result.isDefined)
    assert(result.get.range == 2)
  }

  it should "have head w_te_2_at_U_a(U)" in {
    val rule = convertedRule(atWindow)

    val head = rule.rule.head

    val predicate = head.predicate
    assert(predicate.caption == "w_te_2_at_U_a")
    assert(head.variables == Set(U))
  }

  it should "contain atom at_a(T)" in {
    val rule = convertedRule(atWindow).rule
    val reference = PinnedAtom(a,T)
    assert(rule.body.contains(reference))
  }

  val diamondWindow = WindowAtom(SlidingTimeWindow(2), Diamond, a)

  "A w^2 D a" should "generate one rule" in {
    val result = convert.derivation(diamondWindow)

    assert(result.size == 1)
  }

  it should "have head w_te_2_d_a" in {
    val rule = convertedRule(diamondWindow).rule

    val head = rule.head

    val predicate = head.predicate
    assert(predicate.caption == "w_te_2_d_a")
    assert(head.variables == Set())
  }

  it should "contain atom at_a(U)" in {
    val rule = convertedRule(diamondWindow).rule
    val reference = PinnedAtom(a, U)
    assert(rule.body.contains(reference))
  }

  val boxWindow = WindowAtom(SlidingTimeWindow(2), Box, a)

  "A w^2 B a" should "generate one rule" in {
    val result = convert.derivation_(boxWindow)

    assert(result.size == 1)
  }

  it should "have head w_te_2_b_a" in {
    val rule = convertedRule(boxWindow).rule

    val head = rule.head

    val predicate = head.predicate
    assert(predicate.caption == "w_te_2_b_a")
    assert(head.variables == Set())
  }

  it should "contain atom at_a(T), at_a(T-1)" in {
    val rule = convertedRule(boxWindow).rule
    val reference = a.asAtReference(T)
    assert(rule.body.contains(reference))
    assert(rule.body.contains(a.asAtReference(T - 1)))
  }

}
