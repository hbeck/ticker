package evaluation.diss.programs

import core._
import core.lars.{LarsProgram, LarsRule}
import evaluation.diss.Helpers._
import evaluation.diss.programs.properties.AppliedProgramProvider


/**
  * Created by hb on 18.05.18.
  *
  */
trait ContentProgramProvider extends AppliedProgramProvider {

  def nrOfItems: Int
  def nrOfQLevels: Int

  val item: Predicate = "item"
  val node: Predicate = "node"
  val req: Predicate = "req"
  val cache: Predicate = "cache"
  val need: Predicate = "need"
  val avail: Predicate = "avail"
  val getFrom: Predicate = "getFrom"
  val dism: Predicate = "dism"
  val src: Predicate = "src"
  val worseThan: Predicate = "worseThan"
  val minQ: Predicate = "minQ"
  val nMinQ: Predicate = "nMinQ"
  val qLev: Predicate = "qLev"
  val qual: Predicate = "qual"

  val I: Variable = Variable("I")
  val N: Variable = Variable("N")
  val N2: Variable = Variable("N2")
  val M: Variable = Variable("M")
  val M2: Variable = Variable("M2")
  val Q: Variable = Variable("Q")
  val Q2: Variable = Variable("Q2")

  def program(): LarsProgram = {
    val facts: Set[LarsRule] = {
      (1 to scale).map( n => fact(node(f"n$n")) ) ++
      (1 to nrOfItems).map( i => fact(item(f"i$i")) ) ++
      (1 to nrOfQLevels).map(q => fact(qLev(q)) )
    }.toSet

    val k = windowSize

    LarsProgram.from(facts) ++
    LarsProgram.from(
      need(I,N) <= item(I) and node(N) and wt_D(k,req(I,N)),
      avail(I,N) <= item(I) and node(N) and wt_D(k,cache(I,N)),
      src(I,N,M) <= need(I,N) not avail(I,N) and avail(I,M) and Neq(N,M),
      getFrom(I,N,M) <= src(I,N,M) not dism(I,N,M),
      dism(I,N,M) <= node(M) and getFrom(I,N,M2) and Neq(M,M2),
      dism(I,N,M) <= src(I,N,M) and src(I,N,M2) and worseThan(M,M2),
      worseThan(N,N2) <= minQ(N,Q) and minQ(N2,Q2) and Neq(N,N2) and Lt(Q,Q2),
      minQ(N,Q) <= node(N) and qLev(Q) and wt_D(k,qual(N,Q)) not nMinQ(N,Q),
      nMinQ(N,Q) <= node(N) and qLev(Q) and qLev(Q2)
        and wt_D(k,qual(N,Q)) and wt_D(k,qual(N,Q2)) and Lt(Q2,Q)
    )
  }

}
