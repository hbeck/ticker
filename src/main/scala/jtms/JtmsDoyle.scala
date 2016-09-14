package jtms

import core._
import core.asp.{NormalProgram, NormalRule}

import scala.util.Random

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

  var doSelfSupportCheck = true
  var doConsistencyCheck = true //detect wrong computation of odd loop, report inconsistency

  //for inspection:
  var doJtmsSemanticsCheck = true //for debugging

  var failed = false

  override def update(atoms: Predef.Set[Atom]): Unit = {

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom,Status,String)]()

    try {
      updateDoyle(atoms)

      checkJtmsSemantics()
      checkSelfSupport()
      checkConsistency()
    } catch {
      case e:IncrementalUpdateFailureException => {
        invalidateModel()
      }
    }

  }

  def updateDoyle(atoms: Predef.Set[Atom]): Unit = {
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
      retractionsAffected = retractionsAffected + 1
      //val aff = shuffle(affected(a) + a) //TODO no test coverage
      val aff = affected(a) + a
      aff foreach setUnknown
      aff foreach chooseStatus
    }
  }

  var retractionsAffected = 0

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

  def sortByForcedOrder(atoms: Predef.Set[Atom]): Seq[Atom] = {
    val atomList = Seq[Atom]() ++ atoms

    def sort(a:Atom,b:Atom): Boolean = {
      if (!choiceSeq.contains(b)) return true
      if (!choiceSeq.contains(a)) return false
      choiceSeq.indexOf(a) <= choiceSeq.indexOf(b)
    }

    atomList sortWith sort
  }

  def shuffle(atoms: Set[Atom]): Seq[Atom] = random.shuffle(atoms.toSeq)

  def checkJtmsSemantics(): Unit = {
    if (!doJtmsSemanticsCheck) return
    if (atomsNeedingSupp exists (supp(_).isEmpty)) {
      throw new RuntimeException("model: "+getModel()+"\nno support for atoms "+(atomsNeedingSupp filter (supp(_).isEmpty)))
    }
  }

  def checkSelfSupport(): Unit = {
    if (!doSelfSupportCheck) return
    if (inAtoms exists unfoundedSelfSupport) {
      //throw new RuntimeException("model: "+getModel()+"\nself support exists")
      Console.err.println("model: "+getModel()+"\nself support exists")
      failed = true
      invalidateModel()
    }
  }

  def checkConsistency(): Unit = {
    if (!doConsistencyCheck) return
    if ((inAtoms diff factAtoms) exists (a => !(justifications(a) exists valid))) {
      //throw new RuntimeException("model: "+getModel()+"\ninconsistent state: in-atom has no valid justification")
      Console.err.println("model: "+getModel()+"\ninconsistent state: in-atom has no valid justification")
      failed = true
      invalidateModel()
    }
    if ((outAtoms diff factAtoms) exists (a => (justifications(a) exists valid))) {
      //throw new RuntimeException("model: "+getModel()+"\ninconsistent state: out-atom has valid justification")
      Console.err.println("model: "+getModel()+"\ninconsistent state: out-atom has valid justification")
      failed = true
      invalidateModel()
    }
  }

  def selfSupport(a:Atom): Boolean = supp(a) contains a

  def unfoundedSelfSupport(a: Atom): Boolean = {
    if (!selfSupport(a)) return false
    justifications(a) filter valid exists (r => !(r.pos contains a))
  }


}