package engine

import core.{Atom, AtomWithArguments}
import core.asp.AspFact
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import engine.asp.now
import core.lars.{T, TimePoint}
import engine.asp.evaluation.AspEvaluationEngine

/**
  * Created by FM on 16.05.16.
  */
class AspEvaluationEngineSpec extends FlatSpec {

  val t0 = TimePoint(0)
  val t1 = TimePoint(1)

  val a = Atom("a")

  "An empty model" should "be empty afterwards" in {
    AspEvaluationEngine.removeAtoms(t1, Set()) should have size 0
  }
  "now(T)" should "be removed from result-model" in {
    AspEvaluationEngine.removeAtoms(t1, Set(now(T))) should have size 0
  }
  "now(t1)" should "be removed from result-model" in {
    AspEvaluationEngine.removeAtoms(t1, Set(now(t1))) should have size 0
  }

  "An atom 'a'" should "be part of the result" in {
    AspEvaluationEngine.removeAtoms(t1, Set(a)) should contain only (a)
  }
  "An atom 'a(bar)'" should "be part of the result" in {
    AspEvaluationEngine.removeAtoms(t1, Set(a("bar"))) should contain only (a("bar"))
  }

  "An atom 'a(t1)'" should "be part of the result" in {
    // TODO: discuss if we want to get rid of the time-parameter as well
    AspEvaluationEngine.removeAtoms(t1, Set(a(t1))) should contain only (a(t1))
  }
  it should "be part of the result when parsed from clingo" in {
    // TODO: discuss if we want to get rid of the time-parameter as well
    AspEvaluationEngine.removeAtoms(t1, Set(AtomWithArguments(a, Seq(t1.toString)))) should contain only (a(t1))
  }
  "An atom 'a(t0)'" should "not be part of the result at t1" in {
    // TODO: how can we be sure that the last argument is a time-paramter?
    AspEvaluationEngine.removeAtoms(t1, Set(a(t0))) should have size 0
  }
  it should "also be removed when the atom is parsed from clingo" in {
    AspEvaluationEngine.removeAtoms(t1, Set(AtomWithArguments(a, Seq(t0.toString)))) should have size 0
  }

  "A model containing a(1), now(0) and a(2)" should "be empty at t0" in {
    val model = Set(a("1"), now("0"), a("2"))
    AspEvaluationEngine.removeAtoms(t0, model) should have size 0
  }
}
