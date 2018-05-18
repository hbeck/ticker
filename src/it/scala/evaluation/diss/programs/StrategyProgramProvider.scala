package evaluation.diss.programs

import core.not
import core._
import core.lars.{AtAtom, ExtendedAtom, LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.programs.traits.AppliedProgramProvider

//ground only relevant with Lt
trait StrategyProgramProvider extends AppliedProgramProvider {

  val value: Predicate = "value"
  val nMax: Predicate = "nMax"
  val max: Predicate = "max"
  val third: Predicate = "third"
  val upper: Predicate = "upper"
  val lower: Predicate = "lower"
  val middle: Predicate = "middle"
  val alpha: Predicate = "alpha"
  val high: Atom = "high"
  val mid: Atom = "mid"
  val low: Atom = "low"
  val lfu: Atom = "lfu"
  val lru: Atom = "lru"
  val fifo: Atom = "fifo"
  val specific: Atom = "specific"
  val randomAtom: Atom = "random"

  val M: Variable = Variable("M")
  val V: Variable = Variable("V")
  val V2: Variable = Variable("V2")
  val X: Variable = Variable("X")
  val Y: Variable = Variable("Y")
  val T: Variable = Variable("T")

  def value(i: Int): Atom = f"value($i)"

  def program(): LarsProgram = {
    val facts: Set[LarsRule] = (1 to scale).map{ i => fact(value(i)) }.toSet
    LarsProgram.from(facts) ++
    LarsProgram.from(
      nMax(V) <= value(V) and value(V2) and Gt(V2,V),
      max(V) <= value(V) not nMax(V),
      third(V) <= value(V) and max(M) and Divide(M,IntValue(3),V),
      upper(V) <= value(V) and third(X) and value(Y) and Times(X,IntValue(2),Y) and Lt(Y,V),
      lower(V) <= value(V) and third(X) and Leq(V,X),
      middle(V) <= value(V) not upper(V) not lower(V),
      AtAtom(T,high) <= wt_At(windowSize,T,alpha(V)) and upper(V),
      AtAtom(T,mid) <= wt_At(windowSize,T,alpha(V)) and middle(V),
      AtAtom(T,low) <= wt_At(windowSize,T,alpha(V)) and lower(V),
      lfu <= wt_B(windowSize, high),
      lru <= wt_B(windowSize, mid),
      fifo <= wt_B(windowSize, low),
      specific <= lfu,
      specific <= lru,
      specific <= fifo,
      randomAtom <= not[ExtendedAtom](specific)
    )
  }

}
