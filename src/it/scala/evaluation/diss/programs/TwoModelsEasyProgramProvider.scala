package evaluation.diss.programs

import core.{Atom, IntValue, Predicate}
import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.PreparedAtoms._
import evaluation.diss.programs.AnalyticProgramProvider.makeWindowAtom

//ground only relevant
trait TwoModelsEasyProgramProvider extends AnalyticProgramProvider with Scalable {

  val qX: Atom = "q(X)"
  val gX: Atom = "g(X)"
  val aX: Atom = "a(X)"
  val pX: Atom = "p(X)"
  val nX: Atom = "n(X)"

  def program(): LarsProgram = {
    val waX = makeWindowAtom(winMod,windowSize,aX)
    val facts: Set[LarsRule] = (1 to scale).map{ i => fact(g(i)) }.toSet
    LarsProgram.from(facts) ++
    LarsProgram.from(
      qX <= gX and waX,
      pX <= qX not nX,
      nX <= qX not pX
    )
  }

  //X -> a(X)
  def a(i: Int) = Atom(Predicate("a"),Seq(IntValue(i)))
  val signalsMap: Map[Int,Atom] = (1 to scale).map{ i => (i,a(i)) }.toMap
  val signals = signalsMap.values.toSeq



}
