package evaluation.diss.instances

import core.{Atom, Model}
import evaluation.diss.Helpers._
import evaluation.diss.Instance
import evaluation.diss.PreparedAtoms.{a, b}
import evaluation.diss.programs.traits.Analytic._
import evaluation.diss.programs.BasicProgramProvider

/**
  * Created by hb on 16.04.18.
  *
  * winModKey: window and modality indicator: {ta,td,tb,ca,cd,cb}
  */
case class BasicInstance(winModKey: String, windowSize: Int, signalEvery: Int) extends Instance with BasicProgramProvider {

  assert(signalEvery > 0)

  val winMod = winModFromString(winModKey)

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t % signalEvery == 0) {
      Seq[Atom](b)
    } else {
      Seq()
    }
  }  

  override def verify_time_at(model: Model, t: Int): Unit = {
    verify_time_diamond(model,t)
  }

  override def verify_time_diamond(model: Model, t: Int): Unit = {
    if (t % signalEvery <= windowSize) {
      mustHave(model,a,t)
    } else {
      mustNotHave(model,a,t)
    }
  }

  override def verify_time_box(model: Model, t: Int): Unit = {
    if (t == 0) {
      mustHave(model,a,t)
    } else if (signalEvery == 1) {
      mustHave(model,a,t)
    } else {
      mustNotHave(model,a,t)
    }
  }

  override def verify_count_at(model: Model, t: Int): Unit = {
    verify_count_diamond(model,t)
  }

  override def verify_count_diamond(model: Model, t: Int): Unit = {
    mustHave(model,a,t)
  }

  override def verify_count_box(model: Model, t: Int): Unit = {
    if (t == 0) {
      mustHave(model,a,t)
    } else if (signalEvery == 1) {
      mustHave(model,a,t)
    } else {
      mustNotHave(model,a,t)
    }
  }



}
