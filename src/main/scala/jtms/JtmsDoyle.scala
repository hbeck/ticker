package jtms

import java.util

import scala.util.Random

import core._
import core.asp.{NormalRule, NormalProgram}

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
case class JtmsDoyle(random: Random = new Random()) extends JtmsAbstraction {

  override def update(atoms: Set[Atom]): Unit = {

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom,Status,String)]()

    try {
      updateDoyle(atoms)

//      checkJtmsSemantics()
//      checkSelfSupport()
//      checkConsistency()
    } catch {
      case e:IncrementalUpdateFailureException => {
        invalidateModel()
      }
    }

  }

  def updateDoyle(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown //Marking the nodes
    atoms foreach findStatus // Evaluating the nodes' justifications

    if (doForceChoiceOrder) { // Relaxing circularities (might lead to contradictions)
    val atomList = sortByForcedOrder(atoms)
      atomList foreach chooseStatus
    } else {
      atoms foreach chooseStatus
    }
    //omit backtracking part
  }

  def chooseStatus(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (choice(a)) {
      if (recordChoiceSeq) choiceSeq = choiceSeq :+ a
      unknownCons(a) foreach chooseStatus
    } else {
      val aff = shuffleSeq(Seq[Atom]() ++ affected(a).toSet :+ a) //TODO no test coverage
      aff foreach setUnknown
      aff foreach chooseStatus
    }
  }

  def choice(a: Atom): Boolean = {
    justifications(a) find posValid match {
      case Some(rule) => {
          if (affected(a).isEmpty) setIn(rule)
          else return false
      }
      case None => setOut(a) //allowing 'unknown' instead of 'out' in spoiler!
    }
    true
  }

  /* allows also an unknown atom instead of an out atom, but only if necessary.
     this way, no further heuristic (i.e. losses of potential solutions) are introduced.
     in particular, the method can be used both in the deterministic step
     (which seeks a spoiler only for a (well-founded) invalid rule, i.e., no unknown atom occurs.)
     as well as in the choice step, where an unknown atom might indeed be needed.
   */
  override def findSpoiler(rule: NormalRule): Option[Atom] = {
    if (random.nextDouble() < 0.5) {
      rule.pos find (status(_) == out) match {
        case None => rule.neg find (status(_) == in) match {
          case None => rule.pos find (status(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    } else {
      rule.neg find (status(_) == in) match {
        case None => rule.pos find (status(_) == out) match {
          case None => rule.pos find (status(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    }
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

  def shuffleSeq(atoms: Seq[Atom]): Seq[Atom] = {
    val list = new util.ArrayList[Atom]()
    atoms foreach list.add
    java.util.Collections.shuffle(list)
    var seq = Seq[Atom]()
    for (i <- 0 to list.size()-1) {
      seq = seq :+ list.get(i)
    }
    seq
  }


}