package evaluation.diss.instances

import core.Atom
import evaluation.diss.Instance
import evaluation.diss.PreparedAtoms.string2Atom
import evaluation.diss.programs.AnalyticProgramProvider._
import evaluation.diss.programs.{NoVerification, RandomProvider, ReachAvailProgramProvider}

import scala.util.Random

/**
  * Created by hb on 01.05.18.
  *
  * wm: window and modality indicator: {ta,td,tb,ca,cd,cb}
  * scale: nr of nodes
  * availSignalProbability: at each time point, for each edge(X,Y) an atom avail(X,Y) is generated with the given probability
  * percentNoFailure: lower tier of node indexes that do *not* get a fail signal. measure of model maintainability.
  */
case class ReachAvailInstance(random: Random, wm: String, windowSize: Int, scale: Int, availSignalProb: Double, failSignalEvery: Int, percentNoFailure: Int) extends Instance with ReachAvailProgramProvider with RandomProvider with NoVerification {

  assert(failSignalEvery >= 0)
  assert(scale > 0)
  assert(percentNoFailure >= 0)
  assert(percentNoFailure <= 100)

  val offset:Int = (0.01*percentNoFailure*scale).toInt //e.g. 30% of 200 = 60
  val shifterUpperBound = scale-offset //140 --> 0..139. actual range adds offset

  val winMod = winModFromString(wm)

  var currFailAtom: Option[Atom] = None

  def avail(i: Int, j: Int): Atom = f"avail($i,$j)"
  val availAtoms: Map[Int,Atom] = (1 to scale).map{ i => (i,avail(i-1,i)) }.toMap

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    val pick = (1 to scale).map{ i => (i,random.nextDouble() <= availSignalProb) }.toMap
    val availSeq = availAtoms.filterKeys(pick(_)).values.toSeq
    val failSignalSeq = if (failSignalEvery > 0 && t % failSignalEvery == 0) {
      val i = random.nextInt(shifterUpperBound+1)+offset //--> (0+60..140+60) = 60..200 [fail(0)..fail(59) are not produced. last edge (200,201)]
      val atom: Atom = f"fail($i)"
      currFailAtom = Some(atom)
      Seq[Atom](atom)
    } else {
      currFailAtom = None
      Seq()
    }
    availSeq ++ failSignalSeq
  }

}
