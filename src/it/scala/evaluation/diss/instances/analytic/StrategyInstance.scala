package evaluation.diss.instances.analytic

import core.Atom
import evaluation.diss.Helpers.string2Atom
import evaluation.diss.instances.traits.{Instance, Randomized}
import evaluation.diss.programs.StrategyProgramProvider
import reasoner.Result

import scala.util.Random

/**
  * Created by hb on 18.05.18.
  *
  * valueChangeProbability: probability that alpha value changes from one timepoint to the next to +- 1
  */
case class StrategyInstance(random:Random, windowSize: Int, scale: Int, valueChangeProbability: Double) extends Instance with StrategyProgramProvider with Randomized {

  assert(scale > 0)
  assert(valueChangeProbability >= 0.0)

  var currentValue = random.nextInt(scale) + 1

  def alpha(v: Int): Atom = f"alpha($v)"
  val alphaAtoms: Map[Int,Atom] = (1 to scale).map{ v => (v,alpha(v)) }.toMap

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (random.nextDouble() <= valueChangeProbability) {
      changeCurrentValue()
    }
    Seq(alphaAtoms(currentValue))
  }

  private def changeCurrentValue(): Unit = {
    if (currentValue == 1) {
      currentValue = currentValue + 1
    } else if (currentValue == scale) {
      currentValue = currentValue - 1
    } else {
      val plus = random.nextBoolean()
      if (plus) {
        currentValue = currentValue + 1
      } else {
        currentValue = currentValue - 1
      }
    }
  }

  override def verifyOutput(result: Result, t: Int): Unit = {}

}
