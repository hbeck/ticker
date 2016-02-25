package aspsamples

import core._
import jtms.{jTmn, TMN}
import org.scalatest.FlatSpec

import scala.language.postfixOps

/**
  * Created by FM on 25.02.16.
  * “*/
trait SingleHusbandBehavior {
  this: FlatSpec =>

  val man = Atom("man")
  val single = Atom("single")
  val husband = Atom("husband")

  val r0 = Premise(man)
  val r1 = Rule.in(man).out(husband).head(single)
  val r2 = Rule.in(man).out(single).head(husband)

  val program = Program(r0, r1, r2)

  def singleHusband(evaluation: Evaluation) {
    /*
  man.
single :- man, not husband.
husband :- man, not single.
   */


    it should "include the model man, single" in {
      assert(evaluation(program).get contains Set(man, single))
    }

    it should "include man, husband" in {
      if (evaluation.isInstanceOf[jTmn])
        pending
      assert(evaluation(program).get contains Set(man, husband))
    }
  }
}

class SingleHusbandSample extends FlatSpec with SingleHusbandBehavior with EvaluateBothImplementations {
  "The Single-Husband Sample" should behave like theSame(singleHusband)
}