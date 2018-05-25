package evaluation.diss.instances

import evaluation.diss.Helpers.string2Atom
import core.Atom
import evaluation.diss.Helpers.mustHave
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
      val currentBucket = bucketFor(currentValue)
      changeCurrentValue()
      if (currentBucket != bucketFor(currentValue)) {
        currentBucketSinceTimePoint = t
      }
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

  sealed trait Bucket
  case object Upper extends Bucket
  case object Middle extends Bucket
  case object Lower extends Bucket

  val aThird: Int = scale / 3

  def bucketFor(v: Int): Bucket = {
    if (v<=aThird) {
      Lower
    } else if (v > 2*aThird) {
      Upper
    } else {
      Middle
    }
  }

  var currentBucketSinceTimePoint:Int = 1

  override def verifyOutput(result: Result, t: Int): Unit = {
    if (t < windowSize+1) return //skip initial segment
    if (t - currentBucketSinceTimePoint >= windowSize) {
      bucketFor(currentValue) match {
        case Upper => mustHave(result.model,lfu,t)
        case Middle => mustHave(result.model,lru,t)
        case Lower => mustHave(result.model,fifo,t)
      }
    } else {
      mustHave(result.model,randomAtom,t)
    }
  }
}
