package aspsamples

import asp.{ClingoWrapper, Asp}
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
  val r1 = Rule.pos(man).neg(husband).head(single)
  val r2 = Rule.pos(man).neg(single).head(husband)

  val program = Program(r0, r1, r2)

  def singleHusband(evaluation: => Evaluation) {
    /*
  man.
single :- man, not husband.
husband :- man, not single.
   */


    it should "include the model man, single" in {
      assert(evaluation(program) contains Set(man, single))
    }

    it should "include man, husband" in {
      if (evaluation.isInstanceOf[jTmn])
        pending
      assert(evaluation(program) contains Set(man, husband))
    }
  }
}

class SingleHusbandSample extends FlatSpec with SingleHusbandBehavior {
  def tmn = new jTmn

  def asp = Asp()

  "The TMN implementation " should behave like singleHusband(tmn)
  "The ASP implementation " should behave like singleHusband(asp)
}