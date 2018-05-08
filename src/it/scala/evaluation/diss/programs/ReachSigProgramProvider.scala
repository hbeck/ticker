package evaluation.diss.programs

import core.{Atom, Lt}
import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.Prepared._
import evaluation.diss.programs.traits.AnalyticProgramProvider.makeWindowAtom
import evaluation.diss.programs.traits.{AnalyticProgramProvider, Scalable}

//ground only relevant with Lt
trait ReachSigProgramProvider extends AnalyticProgramProvider with Scalable {

  val reachXY: Atom = "reach(X,Y)"
  val reachYZ: Atom = "reach(Y,Z)"
  val reachXZ: Atom = "reach(X,Z)"
  val edgeXY: Atom = "edge(X,Y)"
  val sigXY: Atom = "sig(X,Y)"

  def edge(i: Int, j: Int): Atom = f"edge($i,$j)"

  def program(): LarsProgram = {
    val wSigXY = makeWindowAtom(winMod,windowSize,sigXY)
    val facts: Set[LarsRule] = (1 to scale).map{ i => fact(edge(i-1,i)) }.toSet
    LarsProgram.from(facts) ++
    LarsProgram.from(
      reachXY <= edgeXY and wSigXY and Lt(X,Y),
      reachXZ <= reachXY and reachYZ and Lt(X,Y) and Lt(Y,Z)
    )

  }

}
