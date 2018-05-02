package evaluation.diss.instances.traits

import core.Model
import evaluation.diss.programs.traits.AnalyticProgramProvider
import evaluation.diss.programs.traits.AnalyticProgramProvider._
import reasoner.Result

trait AnalyticInstance extends Instance with AnalyticProgramProvider {

  override def verifyOutput(result: Result, t: Int): Unit = {
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

  def verify_time_at(model: Model, t: Int):Unit

  def verify_time_diamond(model: Model, t: Int): Unit

  def verify_time_box(model: Model, t: Int): Unit

  def verify_count_at(model: Model, t: Int): Unit

  def verify_count_diamond(model: Model, t: Int)

  def verify_count_box(model: Model, t: Int): Unit

}
