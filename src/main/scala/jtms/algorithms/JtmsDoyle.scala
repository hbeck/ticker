package jtms.algorithms

import core._
import core.asp.{NormalProgram, NormalRule}
import jtms._

import scala.util.Random

object JtmsDoyle {

  def apply(P: NormalProgram): JtmsDoyle = {
    val tmn = new JtmsDoyle(TruthMaintenanceNetwork())
    P.rules foreach tmn.add
    tmn
  }

  def apply(): JtmsDoyle = JtmsDoyle(TruthMaintenanceNetwork())

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
case class JtmsDoyle(net: TruthMaintenanceNetwork, random: Random = new Random()) extends JtmsUpdateAlgorithmAbstraction(net, random) {

  var doSelfSupportCheck = false
  var doConsistencyCheck = false //detect wrong computation of odd loop, report inconsistency

  //for inspection:
  var doJtmsSemanticsCheck = false //for debugging

  var failed = false

  override def update(atoms: Predef.Set[Atom]): Unit = {

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom, Status, String)]()

    try {
      updateDoyle(atoms)

      checkJtmsSemantics()
      checkSelfSupport()
      checkConsistency()
    } catch {
      case e: IncrementalUpdateFailureException => {
        invalidateModel()
      }
    }

  }

  def updateDoyle(atoms: Predef.Set[Atom]): Unit = {
    atoms foreach setUnknown //Marking the nodes
    atoms foreach findStatus // Evaluating the nodes' justifications

    if (doForceChoiceOrder) {
      // Relaxing circularities (might lead to contradictions)
      val atomList = sortByForcedOrder(atoms)
      atomList foreach chooseStatus
    } else {
      atoms foreach chooseStatus
    }
    //omit backtracking part
  }

  def chooseStatus(a: Atom): Unit = {
    if (net.status(a) != unknown)
      return

    if (choice(a)) {
      if (recordChoiceSeq) choiceSeq = choiceSeq :+ a
      net.unknownCons(a) foreach chooseStatus
    } else {
      retractionsAffected = retractionsAffected + 1
      val aff = shuffle(net.affected(a) + a) //TODO no test coverage
      //val aff = affected(a) + a
      aff foreach setUnknown
      aff foreach chooseStatus
    }
  }

  var retractionsAffected = 0

  def choice(a: Atom): Boolean = {
    net.justifications(a) find net.posValid match {
      case Some(rule) => {
        if (net.affected(a).isEmpty) setIn(rule)
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
      rule.pos find (net.status(_) == out) match {
        case None => rule.neg find (net.status(_) == in) match {
          case None => rule.pos find (net.status(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    } else {
      rule.neg find (net.status(_) == in) match {
        case None => rule.pos find (net.status(_) == out) match {
          case None => rule.pos find (net.status(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    }
  }

  def sortByForcedOrder(atoms: Predef.Set[Atom]): Seq[Atom] = {
    val atomList = Seq[Atom]() ++ atoms

    def sort(a: Atom, b: Atom): Boolean = {
      if (!choiceSeq.contains(b)) return true
      if (!choiceSeq.contains(a)) return false
      choiceSeq.indexOf(a) <= choiceSeq.indexOf(b)
    }

    atomList sortWith sort
  }

  def shuffle(atoms: Set[Atom]): Seq[Atom] = random.shuffle(atoms.toSeq)

  def checkJtmsSemantics(): Unit = {
    if (!doJtmsSemanticsCheck) return
    if (net.atomsNeedingSupp exists (net.supp(_).isEmpty)) {
      throw new RuntimeException("model: " + getModel() + "\nno support for atoms " + (net.atomsNeedingSupp filter (net.supp(_).isEmpty)))
    }
  }

  def checkSelfSupport(): Unit = {
    if (!doSelfSupportCheck) return
    if (net.inAtoms exists unfoundedSelfSupport) {
      //throw new RuntimeException("model: "+getModel()+"\nself support exists")
      Console.err.println("model: " + getModel() + "\nself support exists")
      failed = true
      invalidateModel()
    }
  }

  def checkConsistency(): Unit = {
    if (!doConsistencyCheck) return
    if ((net.inAtoms diff net.factAtoms) exists (a => !(net.justifications(a) exists net.valid))) {
      //throw new RuntimeException("model: "+getModel()+"\ninconsistent state: in-atom has no valid justification")
      Console.err.println("model: " + getModel() + "\ninconsistent state: in-atom has no valid justification")
      failed = true
      invalidateModel()
    }
    if ((net.outAtoms diff net.factAtoms) exists (a => (net.justifications(a) exists net.valid))) {
      //throw new RuntimeException("model: "+getModel()+"\ninconsistent state: out-atom has valid justification")
      Console.err.println("model: " + getModel() + "\ninconsistent state: out-atom has valid justification")
      failed = true
      invalidateModel()
    }
  }

  def selfSupport(a: Atom): Boolean = net.supp(a) contains a

  def unfoundedSelfSupport(a: Atom): Boolean = {
    if (!selfSupport(a)) return false
    net.justifications(a) filter net.valid exists (r => !(r.pos contains a))
  }
}