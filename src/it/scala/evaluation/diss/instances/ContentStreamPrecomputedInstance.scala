package evaluation.diss.instances

import core.Atom
import evaluation.diss.instances.traits.{Instance, InstanceSansVerification, Randomized}
import evaluation.diss.programs.ContentProgramProvider

import scala.util.Random

/**
  * Created by hb on 18.05.18.
  *
  */
case class ContentStreamPrecomputedInstance(random: Random, windowSize: Int, scale: Int, nrOfItems: Int, nrOfQLevels: Int, pCache: Double, pQLevChange: Double, timepoints: Int) extends Instance with ContentProgramProvider with Randomized with InstanceSansVerification {

  val contentInstance = ContentInstance(random,windowSize,scale,nrOfItems,nrOfQLevels,pCache,pQLevChange)

  val stream: Map[Int,Seq[Atom]] = (0 to timepoints).map(t => (t,contentInstance.generateSignalsToAddAt(t))).toMap

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    stream(t)
  }

}
