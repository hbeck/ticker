package evaluation.diss.instances.analytic

import core.Atom
import evaluation.diss.instances.traits.{Instance, InstanceSansVerification, Randomized}
import evaluation.diss.programs.ContentProgramProvider

import scala.util.Random

/**
  * Created by hb on 18.05.18.
  *
  */
case class ContentInstance(random: Random, windowSize: Int, scale: Int, nrOfItems: Int, nrOfQLevels: Int, pCache: Double, pQLevChange: Double) extends Instance with ContentProgramProvider with Randomized with InstanceSansVerification {

  assert(scale > 0)
  assert(nrOfItems > 0)
  assert(nrOfQLevels > 0)

  val nrOfNodes = scale

  //keys: node indexes
  var currentQLevel: Map[Int,Int] = (1 to nrOfNodes).map(n => (n,random.nextInt(nrOfQLevels)+1)).toMap

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    changedQualityLevels() ++ caches() ++ requests()
  }

  //each node signals with probability pQLevChange to +- 1 (each 0.5; 1.0 at borders)
  def changedQualityLevels(): Seq[Atom] = {
    val nodeIndexes = randomNodeIndexes(pQLevChange)
    nodeIndexes.foreach( n => updateQualityLevel(n) )
    nodeIndexes.map( n => qual(n,currentQLevel(n)) )
  }

  def randomNodeIndexes(p: Double): Seq[Int] = {
    (1 to nrOfNodes).filter{ n => random.nextDouble() <= p }
//    (1 to nrOfNodes).map(n => (n, random.nextDouble() <= p)).collect {
//      case kv if (kv._2) => kv._1
//    }
  }

  def updateQualityLevel(nodeIndex: Int): Unit = {
    val current = currentQLevel(nodeIndex)
    val newQLevel = newQualityLevel(current)
    currentQLevel = currentQLevel.updated(nodeIndex,newQLevel)
  }

  def newQualityLevel(current: Int): Int = {
    if (current == 1) {
      current + 1
    } else if (current == nrOfQLevels) {
      current - 1
    } else {
      val plus = random.nextBoolean()
      if (plus) {
        current + 1
      } else {
        current - 1
      }
    }
  }

  //each node caches with probability pCache a random item
  def caches(): Seq[Atom] = {
    randomNodeIndexes(pCache).map{ n =>
      val i = random.nextInt(nrOfItems) + 1
      cache(i,n)
    }
  }

  def requests(): Seq[Atom] = {
    val i = random.nextInt(nrOfItems)+1
    val n = random.nextInt(nrOfNodes)
    Seq(req(i,n))
  }

}
