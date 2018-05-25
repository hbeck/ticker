package evaluation.diss.instances

import evaluation.diss.Helpers.string2Atom
import core.Atom
import evaluation.diss.instances.traits.{AnalyticInstance, AnalyticInstanceSansVerification, Randomized}
import evaluation.diss.programs.ReachSigProgramProvider
import evaluation.diss.programs.properties.AnalyticProgramProvider.winModFromString

import scala.util.Random

/**
  * Created by hb on 05.05.18.
  *
  * wm: window and modality indicator: {ta,td,tb,ca,cd,cb} - use for tb and cb
  * scale: nr of nodes
  * signalProbability: at each time point, for each edge(X,Y) an atom sig(X,Y) is generated with the given probability
  * 'dual': program has not before window operator
  */
case class ReachSigDualInstance(random:Random, winModKey: String, windowSize: Int, scale: Int, signalProbability: Double) extends AnalyticInstance with ReachSigProgramProvider with Randomized with AnalyticInstanceSansVerification {

  assert(scale > 0)
  assert(signalProbability >= 0.0)

  val winMod = winModFromString(winModKey)

  def sig(i: Int, j: Int): Atom = f"sig($i,$j)"
  val sigAtoms: Map[Int,Atom] = (1 to scale).map{ i => (i,sig(i-1,i)) }.toMap

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    val pick = (1 to scale).map{ i => (i,random.nextDouble() <= signalProbability) }.toMap
    sigAtoms.filterKeys(pick(_)).values.toSeq
  }

}
