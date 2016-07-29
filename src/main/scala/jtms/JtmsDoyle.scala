package jtms

import core._
import core.asp.{NormalProgram, NormalRule}

import scala.collection.mutable.Set

object JtmsDoyle {

  def apply(P: NormalProgram): JtmsDoyle = {
    val tmn = new JtmsDoyle()
    P.rules foreach tmn.add
    tmn
  }

}

/**
  * justification-based truth maintenance network
  *
  * specific algorithm obtained as refactoring from
  * pseudo-code in book chapter from Beierle and Kern-Isberner
  * (who in turn simplify Doyle's original algorithm)
  *
  * Created by hb on 12/22/15.
  */
case class JtmsDoyle() extends JtmsAbstraction {

  override def update(atoms: Set[Atom]): Unit = {

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom,Status,String)]()

    atoms foreach setUnknown //Marking the nodes
    atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications

    if (doForceChoiceOrder) { // Relaxing circularities (might lead to contradictions)
      val atomList = sortByForcedOrder(atoms)
      atomList foreach fixAndPropagateStatus
    } else {
      atoms foreach fixAndPropagateStatus
    }
    //omit backtracking part
  }

  def fixAndPropagateStatus(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (fix(a)) {
      unknownCons(a) foreach fixAndPropagateStatus
    } else {
      val aff = affected(a) + a //TODO no test coverage (impossible!?)
      aff foreach setUnknown
      aff foreach fixAndPropagateStatus
    }
  }

  def fix(a: Atom): Boolean = {
    justifications(a) find posValid match {
      case Some(rule) => {
          if (affected(a).isEmpty) fixIn(rule) //TODO !
          else return false
      }
      case None => fixOut(a)
    }
    true
  }

  //TODO review
  def fixIn(posValidRule: NormalRule) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (posValidRule.head, in,"fix")
    posValidRule.neg filter (status(_) == unknown) foreach setOut //fix ancestors
    setIn(posValidRule)
  }

  //TODO review
  def fixOut(a: Atom) = {
    status(a) = out
    if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"fix")
    //val unknownPosAtoms = justifications(a) map { r => (r.pos find (status(_)==unknown)).get }
    val maybeAtoms: List[Option[Atom]] = justifications(a) map { r => (r.pos find (status(_)==unknown)) }
    val unknownPosAtoms = (maybeAtoms filter (_.isDefined)) map (_.get)
    unknownPosAtoms foreach setOut //fix ancestors
    setOut(a)
  }

  def sortByForcedOrder(atoms: Set[Atom]): Seq[Atom] = {
    val atomList = Seq[Atom]() ++ atoms

    def sort(a:Atom,b:Atom): Boolean = {
      if (!choiceSeq.contains(b)) return true
      if (!choiceSeq.contains(a)) return false
      choiceSeq.indexOf(a) <= choiceSeq.indexOf(b)
    }

    atomList sortWith sort
  }


}