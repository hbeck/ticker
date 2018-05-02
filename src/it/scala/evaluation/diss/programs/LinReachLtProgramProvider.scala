package evaluation.diss.programs

import core.lars.{LarsProgram, LarsRule}
import core.{Atom, Lt}
import evaluation.diss.Helpers._
import evaluation.diss.PreparedAtoms._
import evaluation.diss.PreparedVariables._
import evaluation.diss.programs.traits.Analytic.makeWindowAtom
import evaluation.diss.programs.traits.{Analytic, Scalable}

trait LinReachLtProgramProvider extends Analytic with Scalable {

  val stepXY: Atom = "step(X,Y)"
  val stepYZ: Atom = "step(Y,Z)"
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
      reachXY <= stepXY and Lt(X,Y),
      reachXZ <= reachXY and stepYZ and Lt(X,Y) and Lt(Y,Z),
      stepXY <= edgeXY and Lt(X,Y) not blockXY,
      blockXY <= edgeXY and wFailX and Lt(X,Y)
    )

  }

}
