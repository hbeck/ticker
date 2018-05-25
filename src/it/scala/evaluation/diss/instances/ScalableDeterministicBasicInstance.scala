package evaluation.diss.instances

import core.{Atom, Model}
import evaluation.diss.Helpers.{mustHave, mustNotHave, string2Atom}
import evaluation.diss.instances.traits.AnalyticInstance
import evaluation.diss.programs.ScalableBasicProgramProvider
import evaluation.diss.programs.properties.AnalyticProgramProvider.winModFromString

/**
  * Created by hb on 16.04.18.
  *
  * winModKey: window and modality indicator: {ta,td,tb,ca,cd,cb}
  */
case class ScalableDeterministicBasicInstance(winModKey: String, windowSize: Int, scale: Int, allSignalsEvery: Int) extends AnalyticInstance with ScalableBasicProgramProvider {

  assert(allSignalsEvery > 0)
  assert(scale > 0)

  val winMod = winModFromString(winModKey)

  def a(i: Int): Atom = f"a($i)"
  def b(i: Int): Atom = f"b($i)"

  val derivations: Seq[Atom] = (1 to scale).map(a(_))
  val signals: Seq[Atom] = (1 to scale).map(b(_))

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t % allSignalsEvery == 0) {
      signals
    } else {
      Seq()
    }
  }

  override def verify_time_at(model: Model, t: Int): Unit = {
    verify_time_diamond(model,t)
  }

  override def verify_time_diamond(model: Model, t: Int): Unit = {
    if (t % allSignalsEvery <= windowSize) {
      derivations.foreach(aX => mustHave(model,aX,t))
    } else {
      derivations.foreach(aX => mustNotHave(model,aX,t))
    }
  }

  override def verify_time_box(model: Model, t: Int): Unit = {
    if (t == 0) {
      derivations.foreach(aX => mustHave(model,aX,t))
    } else if (allSignalsEvery == 1) {
      derivations.foreach(aX => mustHave(model,aX,t))
    } else {
      derivations.foreach(aX => mustNotHave(model,aX,t))
    }
  }

  override def verify_count_at(model: Model, t: Int): Unit = {
    verify_count_diamond(model,t)
  }

  override def verify_count_diamond(model: Model, t: Int): Unit = {
    //partial
    if (model.size < scale - 1) { //ensures tick cut-off within time point is not the problem
      derivations.foreach(aX => mustHave(model, aX, t))
    }
  }

  override def verify_count_box(model: Model, t: Int): Unit = {
    //partial
    if (t > 0 && allSignalsEvery > 1 && model.size < scale - 1) {
      derivations.foreach(aX => mustNotHave(model,aX,t))
    }
  }



}
