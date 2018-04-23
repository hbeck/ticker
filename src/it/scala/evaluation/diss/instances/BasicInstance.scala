package evaluation.diss.instances

import core.{Atom, Model}
import evaluation.diss.Helpers._
import evaluation.diss.Instance
import evaluation.diss.PreparedAtoms.{a, b}
import evaluation.diss.instances.AnalyticCommon._
import evaluation.diss.programs.BasicProgramProvider
import reasoner.Result

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

  def verifyOutput(result: Result, t: Int): Unit = {
    val model = result.model
    winMod match {
      case `time_at` => verify_time_at(model,t)
      case `time_diamond` => verify_time_diamond(model,t)
      case `time_box` => verify_time_box(model,t)
      case `count_at` => verify_count_at(model,t)
      case `count_diamond` => verify_count_diamond(model,t)
      case `count_box` => verify_count_box(model,t)
    }
  }

  private def verify_time_at(model: Model, t: Int): Unit = {
    verify_time_diamond(model,t)
  }

  private def verify_time_diamond(model: Model, t: Int): Unit = {
    if (t % signalEvery <= windowSize) {
      mustHave(model,a,t)
    } else {
      mustNotHave(model,a,t)
    }
  }

  private def verify_time_box(model: Model, t: Int): Unit = {
    if (t == 0) {
      mustHave(model,a,t)
    } else if (signalEvery == 1) {
      mustHave(model,a,t)
    } else {
      mustNotHave(model,a,t)
    }
  }

  private def verify_count_at(model: Model, t: Int): Unit = {
    verify_count_diamond(model,t)
  }

  private def verify_count_diamond(model: Model, t: Int): Unit = {
    mustHave(model,a,t)
  }

  private def verify_count_box(model: Model, t: Int): Unit = {
    if (t == 0) {
      mustHave(model,a,t)
    } else if (signalEvery == 1) {
      mustHave(model,a,t)
    } else {
      mustNotHave(model,a,t)
    }
  }



}
