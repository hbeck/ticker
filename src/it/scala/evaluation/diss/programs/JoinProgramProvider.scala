package evaluation.diss.programs

import core.Atom
import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.instances.AnalyticCommon
import evaluation.diss.PreparedAtoms._
import evaluation.diss.Helpers._

trait JoinProgramProvider extends AnalyticProgramProvider with Scalable {

  val aY: Atom = "a(Y)"
  val bXY: Atom = "b(X,Y)"
  val cYZ: Atom = "c(Y,Z)"

  def program(): LarsProgram = {
    val wbXY = AnalyticCommon.windowAtom(winMod,windowSize,bXY)
    val wcYZ = AnalyticCommon.windowAtom(winMod,windowSize,cYZ)
    val rules = Seq[LarsRule](rule(aY,Set(wbXY,wcYZ,gX,gY,gZ))) ++ ((1 to scale).map{ i => fact(g(i)) })
    LarsProgram(rules)
  }

}
