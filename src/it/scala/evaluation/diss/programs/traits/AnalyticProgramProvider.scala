package evaluation.diss.programs.traits

import core.Atom
import evaluation.diss.Helpers._
import evaluation.diss.Prepared.T
import evaluation.diss.programs.traits.AnalyticProgramProvider.WindowModalityCombi

trait AnalyticProgramProvider extends ProgramProvider {

  def winMod: WindowModalityCombi
  def windowSize: Int

}

object AnalyticProgramProvider {

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