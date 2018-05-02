package evaluation.diss.programs

import core.Atom
import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.Prepared._
import evaluation.diss.programs.traits.AnalyticProgramProvider.makeWindowAtom
import evaluation.diss.programs.traits.{AnalyticProgramProvider, Scalable}

//full grounding
trait ReachProgramProvider extends AnalyticProgramProvider with Scalable {

  val reachXY: Atom = "reach(X,Y)"
  val reachYZ: Atom = "reach(Y,Z)"
  val reachXZ: Atom = "reach(X,Z)"
  val edgeXY: Atom = "edge(X,Y)"
  val blockXY: Atom = "block(X,Y)"
  val failX: Atom = "fail(X)"

  def edge(i: Int, j: Int): Atom = f"edge($i,$j)"

  def program(): LarsProgram = {
    val wFailX = makeWindowAtom(winMod,windowSize,failX)
    val facts: Set[LarsRule] = (1 to scale).map{ i => fact(edge(i-1,i)) }.toSet
    LarsProgram.from(facts) ++
    LarsProgram.from(
      reachXY <= edgeXY not blockXY,
      reachXZ <= reachXY and reachYZ,
      blockXY <= edgeXY and wFailX
    )

  }

}
