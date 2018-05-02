package evaluation.diss.programs

import core.Atom
import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.programs.traits.{AnalyticProgramProvider, Scalable}
import evaluation.diss.programs.traits.AnalyticProgramProvider.makeWindowAtom

trait ScalableBasicProgramProvider extends AnalyticProgramProvider with Scalable {

  val aX: Atom = "a(X)"
  val bX: Atom = "b(X)"
  val gX: Atom = "g(X)"

  def g(i: Int): Atom = f"g($i)"

  def program(): LarsProgram = {
    val wbX = makeWindowAtom(winMod,windowSize,bX)
    val facts: Set[LarsRule] = (1 to scale).map{ i => fact(g(i)) }.toSet
    LarsProgram.from(facts) ++
    LarsProgram.from(aX <= gX and wbX)
  }

}
