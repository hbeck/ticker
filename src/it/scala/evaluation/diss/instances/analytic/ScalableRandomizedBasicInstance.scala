package evaluation.diss.instances.analytic

import core.Atom
import evaluation.diss.Helpers._
import evaluation.diss.instances.traits.{AnalyticInstance, AnalyticInstanceSansVerification, Randomized}
import evaluation.diss.programs.ScalableBasicProgramProvider
import evaluation.diss.programs.properties.AnalyticProgramProvider.winModFromString

import scala.util.Random

/**
  * Created by hb on 02.05.18.
  *
  * winModKey: window and modality indicator: {ta,td,tb,ca,cd,cb}
  */
case class ScalableRandomizedBasicInstance(random:Random, winModKey: String, windowSize: Int, scale: Int, signalProbability: Double) extends AnalyticInstance with Randomized with ScalableBasicProgramProvider with AnalyticInstanceSansVerification {

  assert(scale > 0)

  val winMod = winModFromString(winModKey)

  def b(i: Int): Atom = f"b($i)"

  val signals: Map[Int,Atom] = (1 to scale).map{ i => (i,b(i)) }.toMap

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    val pick = (1 to scale).map{ i => (i,random.nextDouble() <= signalProbability) }.toMap
    val signalSeq = signals.filterKeys(pick(_)).values.toSeq
    signalSeq
  }

}
