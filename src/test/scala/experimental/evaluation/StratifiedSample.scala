package experimental.evaluation

import core.lars._
import core.not
import fixtures._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.Inspectors._

/**
  * Created by FM on 02.06.16.
  */
class StratifiedSample extends ConfigurableEngineSpec with TimeTestFixtures with TmsDirectPolicyEngine {
  val program = LarsProgram.from(
    a <= b and c not d,

    d <= e and W(1, Diamond, f),
    e <= W(1, Box, x),
    f <= W(10, Diamond, y),

    b <= W(10, Box, r),

    c <= not(W(1, Box, s))
  )

  "An empty program" should "lead to model c" in {
    val result = reasoner.evaluate(t0).get.value
    result should contain only (c)
  }

  "Given {0...10 -> r}" should "lead to Model a,  b, c at t10" in {
    (0 to 10) foreach (reasoner.append(_)(r))

    reasoner.evaluate(10).get.value should contain allOf(a, b, c)
  }

  "Given {0...10 -> r, 5 -> y}" should "lead to Model a, b, c ,f at t10" in {
    (0 to 10) foreach (t => {
      reasoner.append(t)(r)
      if (t == 5) reasoner.append(5)(y)
    })

    reasoner.evaluate(10).get.value should contain allOf(a, b, c, f)
  }

  "Given {0...100 -> {r, s}}" should "not lead to a at any time" in {
    (0 to 100) foreach (i => {
      reasoner.append(i)(r, s)

      reasoner.evaluate(TimePoint(i)).get.value shouldNot contain(a)
    })


  }
}

class AllStratified extends RunWithAllImplementations(new StratifiedSample)
