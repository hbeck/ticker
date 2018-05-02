package evaluation.diss.programs

import core.{Atom, Lt}
import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.PreparedAtoms._
import evaluation.diss.PreparedVariables._
import evaluation.diss.programs.traits.Analytic.makeWindowAtom
import evaluation.diss.programs.traits.{Analytic, Scalable}

//ground only relevant
trait ReachAvailProgramProvider extends Analytic with Scalable {

  val reachXY: Atom = "reach(X,Y)"
  val reachYZ: Atom = "reach(Y,Z)"
  val reachXZ: Atom = "reach(X,Z)"
  val edgeXY: Atom = "edge(X,Y)"
  val availXY: Atom = "avail(X,Y)"
  val failX: Atom = "fail(X)"

  def edge(i: Int, j: Int): Atom = f"edge($i,$j)"

  def program(): LarsProgram = {
    val wFailX = makeWindowAtom(winMod,windowSize,failX)
    val facts: Set[LarsRule] = (1 to scale).map{ i => fact(edge(i-1,i)) }.toSet
    LarsProgram.from(facts) ++
    LarsProgram.from(
      reachXY <= edgeXY and availXY and Lt(X,Y) not wFailX,
      reachXZ <= reachXY and reachYZ and Lt(X,Y) and Lt(Y,Z)
    )

  }

}
