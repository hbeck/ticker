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

  val r0 = Fact(man)
  val r1 = Rule(single,Set(man),Set(husband)) //single :- man, not husband
  val r2 = Rule(husband,Set(man),Set(single)) //husband :- man, not single

  val pSingleFirst = Program(r0, r1, r2)
  val pHusbandFirst = Program(r0, r2, r1)

  def singleHusband(evaluation: => Evaluation) {

    /*
        man.
        single :- man, not husband.
        husband :- man, not single.
   */

    it should "include the model man, single" in {
      assert(evaluation(pSingleFirst) contains Set(man, single))
    }

    it should "include the model man, husband" in {
      assert(evaluation(pHusbandFirst) contains Set(man, husband))
    }

  }
}

class SingleHusbandSample extends FlatSpec with SingleHusbandBehavior {
  def tmn = new jTmn

  def asp = Asp()

  "The TMN implementation " should behave like singleHusband(tmn)
  "The ASP implementation " should behave like singleHusband(asp)
}