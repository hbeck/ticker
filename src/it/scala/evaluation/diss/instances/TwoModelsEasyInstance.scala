package evaluation.diss.instances

import core.Atom
import evaluation.diss.Instance
import evaluation.diss.programs.AnalyticProgramProvider._
import evaluation.diss.programs.{NoVerification, RandomProvider, TwoModelsEasyProgramProvider}

import scala.util.Random

/**
  * Created by hb on 01.05.18.
  *
  * wm: window and modality indicator: {ta,td,tb,ca,cd,cb}
  * scale: nr of nodes g(X)
  * signalProb: probability of each a(X) (X=1..scale) to be inserted at a time point
  */
case class TwoModelsEasyInstance(random: Random, wm: String, windowSize: Int, scale: Int, signalProb: Double) extends Instance with TwoModelsEasyProgramProvider with RandomProvider with NoVerification {

  assert(signalProb >= 0.0)
  assert(signalProb <= 1.0)
  assert(scale > 0)

  val winMod = winModFromString(wm)

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (signalProb == 1.0) {
      signals
    } else {
      val pick = (1 to scale).map { i => (i, random.nextDouble() <= signalProb) }.toMap
      signalsMap.filterKeys(pick(_)).values.toSeq
    }
  }

}
