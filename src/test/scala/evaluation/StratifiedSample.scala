package evaluation

import core.lars._
import core.not
import fixtures._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.Inspectors._

/**
  * Created by FM on 02.06.16.
  */
class StratifiedSample extends ConfigurableEvaluationSpec with TimeTestFixtures with TmsPushEngine {
  val program = LarsProgram.from(
    a <= b and c not d,

    d <= e and W(1, Diamond, f),
    e <= W(1, Box, x),
    f <= W(10, Diamond, y),

    b <= W(10, Box, r),

    c <= not(W(1, Box, s))
  )

  "An empty program" should "lead to model c" in {
    evaluationEngine.evaluate(t0).get.value should contain only (c)
  }

  "Given {0...10 -> r}" should "lead to Model a, r, b, c at t10" in {
    (0 to 10) foreach (evaluationEngine.append(_)(r))

    evaluationEngine.evaluate(10).get.value should contain allOf(a, r, b, c)
  }
  "Given {0...10 -> r, 5 -> y}" should "lead to Model a, r, b, c ,f at t10" in {
    (0 to 10) foreach (evaluationEngine.append(_)(r))
    evaluationEngine.append(5)(y)

    evaluationEngine.evaluate(10).get.value should contain allOf(a, r, b, c, f)
  }

  "Given {0...100 -> {r, s}}" should "not lead to a at any time" in {
    (0 to 100) foreach (i => {
      evaluationEngine.append(i)(r, s)

      evaluationEngine.evaluate(TimePoint(i)).get.value shouldNot contain (a)
    })



  }
}

class AllStratified extends RunWithAllImplementations(new StratifiedSample)
