package evaluation.diss.instances

import core.Atom
import evaluation.diss.instances.traits.{Instance, InstanceSansVerification, Randomized}
import evaluation.diss.programs.ContentProgramProvider

import scala.util.Random

/**
  * Created by hb on 18.05.18.
  *
  * Randomize probabilities for each run.
  *
  */
case class ContentRandInstance(random: Random, windowSize: Int, scale: Int, nrOfItems: Int, nrOfQLevels: Int) extends Instance with ContentProgramProvider with Randomized with InstanceSansVerification {

  val pCache = random.nextDouble()
  val pQLevChange = random.nextDouble()

  val contentInstance = ContentInstance(random,windowSize,scale,nrOfItems,nrOfQLevels,pCache,pQLevChange)

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    contentInstance.generateSignalsToAddAt(t)
  }

}
