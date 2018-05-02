package evaluation.diss.programs.traits

import core.{Atom, Model}
import evaluation.diss.Helpers._
import evaluation.diss.PreparedVariables.T
import evaluation.diss.programs.traits.Analytic.WindowModalityCombi
import evaluation.diss.programs.traits.Analytic._
import reasoner.Result

trait Analytic extends ProgramProvider with Verifiable {

  def winMod: WindowModalityCombi
  def windowSize: Int

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

object Analytic {

  sealed abstract class WindowModalityCombi
  object time_at extends WindowModalityCombi
  object time_diamond extends WindowModalityCombi
  object time_box extends WindowModalityCombi
  object count_at extends WindowModalityCombi
  object count_diamond extends WindowModalityCombi
  object count_box extends WindowModalityCombi

  def winModFromString(wm: String) = wm match {
    case "ta" => time_at
    case "td"=> time_diamond
    case "tb" => time_box
    case "ca" => count_at
    case "cd" => count_diamond
    case "cb" => count_box
  }

  def makeWindowAtom(winMod: WindowModalityCombi, windowSize: Int, atom: Atom) = winMod match {
    case `time_at` => wt_At(windowSize,T,atom)
    case `time_diamond` => wt_D(windowSize,atom)
    case `time_box` => wt_B(windowSize,atom)
    case `count_at` => wc_At(windowSize,T,atom)
    case `count_diamond` => wc_D(windowSize,atom)
    case `count_box` => wc_B(windowSize,atom)
  }
}