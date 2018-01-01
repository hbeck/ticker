package engine.examples

import core.lars._
import fixtures._
import org.scalatest.Matchers._
import org.scalatest.Inspectors._
import org.scalatest.OptionValues._

/**
  * Created by FM on 12.08.16.
  */
class ATupleWindowDiamondSample extends ConfigurableEngineSpec with TimeTestFixtures with TmsDirectPolicyEngine {

  /**
    *
    * ******** 7     10  11      14
    * ---------|------|--|--------|----
    * ******** d      f  e        d
    */
  val program = LarsProgram.from(
    a <= WindowAtom(SlidingTupleWindow(2), Diamond, d)
  )

  "An empty program" should "not lead to a at 0" in {
    engine.evaluate(t0).get.value shouldNot contain(a)
  }

  "{7 -> d}" should "lead to a from 7 to 9" in {
    engine.append(7)(d)

    forAll(7 to 9) { i =>
      engine.evaluate(i).get.value should contain(a)
    }
  }

  "{7 -> d, 10 -> f}" should "still lead to a at 10" in {
    engine.append(7)(d)
    engine.append(10)(f)

    engine.evaluate(10).get.value should contain(a)
  }

  "{7 -> d, 10 -> f, 11 -> e}" should "not lead to a from 11 to 14" in {
    engine.append(7)(d)
    engine.append(10)(f)
    engine.append(11)(e)

    // TODO: difference between TMS/Clingo with window-size
    forAll(11 to 14) { i =>
      engine.evaluate(i).get.value shouldNot contain(a)
    }
  }

  "{7 -> d, 10 -> f, 11 -> e, 14 -> d}" should "lead to a from 14" in {
    engine.append(7)(d)
    engine.append(10)(f)
    engine.append(11)(e)
    engine.append(14)(d)

    engine.evaluate(14).get.value should contain(a)
  }

}
