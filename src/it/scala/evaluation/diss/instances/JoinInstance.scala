package evaluation.diss.instances

import core.{Atom, Model}
import evaluation.diss.Helpers._
import evaluation.diss.Instance
import evaluation.diss.PreparedAtoms.string2Atom
import evaluation.diss.programs.AnalyticProgramProvider._
import evaluation.diss.programs.JoinProgramProvider

/**
  * Created by hb on 20.04.18.
  *
  * wm: window and modality indicator: {ta,td,tb,ca,cd,cb}
  * scale: nr of atoms for guards of form g(X)
  */
case class JoinInstance(wm: String, windowSize: Int, signalEvery: Int, scale: Int) extends Instance with JoinProgramProvider {

  assert(signalEvery > 0)
  assert(scale > 0)

  val winMod = winModFromString(wm)

  var lastPredicate_c:Boolean = false //alternative b and c
  var optLastInference: Option[Atom] = None
  var k:Int = 1

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t % signalEvery == 0) {
      if (k > scale) {
        k = 1
      }
      var currAtom:Atom = null
      if (lastPredicate_c) {
        lastPredicate_c = true
        currAtom = f"c($k,${k+1})"
        if (k>1) {
          val inf: Atom = f"a(${k - 1},${k + 1})"
          optLastInference = Some(inf)
        }
      } else {
        lastPredicate_c = false
        currAtom = f"b($k,${k+1})"
      }
      k = k + 1
      Seq[Atom](currAtom)
    } else {
      Seq()
    }
  }

  override def verify_time_at(model: Model, t: Int): Unit = {
    verify_time_diamond(model,t)
  }

  override def verify_time_diamond(model: Model, t: Int): Unit = {
    //partial
    if (t % signalEvery == 0 && lastPredicate_c) { //verify only current entailment
      mustHave(model,optLastInference.get,t)
    }
  }

  override def verify_time_box(model: Model, t: Int): Unit = {
    if (optLastInference.isEmpty) return
    mustNotHave(model,optLastInference.get,t)
  }

  override def verify_count_at(model: Model, t: Int): Unit = {
    verify_count_diamond(model,t)
  }

  override def verify_count_diamond(model: Model, t: Int): Unit = {
    if (optLastInference.isEmpty) return
    //partial
    mustHave(model,optLastInference.get,t)
  }

  override def verify_count_box(model: Model, t: Int): Unit = {
    if (optLastInference.isEmpty) return
    mustNotHave(model,optLastInference.get,t)
  }

}
