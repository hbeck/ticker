package evaluation.diss.programs

import core.Atom
import core.lars.LarsProgram
import evaluation.diss.Helpers._
import evaluation.diss.PreparedAtoms._
import evaluation.diss.programs.AnalyticProgramProvider.makeWindowAtom

trait JoinProgramProvider extends AnalyticProgramProvider with Scalable {

  val aXZ: Atom = "a(X,Z)"
  val bXY: Atom = "b(X,Y)"
  val cYZ: Atom = "c(Y,Z)"

  def program(): LarsProgram = {
    val wbXY = makeWindowAtom(winMod,windowSize,bXY)
    val wcYZ = makeWindowAtom(winMod,windowSize,cYZ)
    LarsProgram.from(aXZ <= wbXY and wcYZ and gX and gY and gZ) ++
    LarsProgram.from((1 to scale).map{ i => fact(g(i)) }.toSet)
  }

}
