package evaluation.diss.programs

import core.Atom
import core.lars.{AtAtom, LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.Prepared.T
import evaluation.diss.programs.traits.{ProgramProvider, Scalable}

trait CarsProgramProvider extends ProgramProvider with Scalable {

  val timeWindowSize: Int
  val k: Int

  val moreThanK: Atom = "moreThanK"
  val x: Atom = "x"
  val carC: Atom = "car(C)"
  val recC: Atom = "rec(C)"

  def car(i: Int): Atom = f"car($i)"

  def program(): LarsProgram = {
    val facts: Set[LarsRule] = (1 to scale).map{ i => fact(car(i)) }.toSet
    LarsProgram.from(facts)
    LarsProgram.from(
      moreThanK <= wt_D(timeWindowSize,x),
      AtAtom(T,x) <= carC and wc_At(k+1,T,recC) not wc_D(k,recC)
    )
  }

}
