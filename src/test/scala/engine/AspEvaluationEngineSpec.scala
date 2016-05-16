package engine

import core.Atom
import core.asp.AspFact
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import engine.asp.now
import core.lars.T
import engine.asp.evaluation.AspEvaluationEngine

/**
  * Created by FM on 16.05.16.
  */
class AspEvaluationEngineSpec extends FlatSpec {

  val a = Atom("a")

  "An empty model" should "be empty afterwards" in{
    AspEvaluationEngine.removeNow(Set()) should have size 0
  }
  "now(T)" should "be removed from result-model" in {
    AspEvaluationEngine.removeNow(Set(now(T))) should have size 0
  }
  "An atom 'a'" should "be part of the result" in{
    AspEvaluationEngine.removeNow(Set(a)) should contain only(a)
  }
  "An atom 'a(T)'" should "be part of the result" in{
    // TODO: discuss if we want to get rid of the T as well
    AspEvaluationEngine.removeNow(Set(a(T))) should contain only(a(T))
  }
}
