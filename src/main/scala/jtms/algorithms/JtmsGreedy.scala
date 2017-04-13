package jtms.algorithms

import core._
import core.asp.{NormalProgram, NormalRule}
import jtms._
import jtms.networks.OptimizedNetwork

import scala.util.Random

object JtmsGreedy {

  def apply(P: NormalProgram): JtmsGreedy = {
    val net = new JtmsGreedy(new OptimizedNetwork())
    P.rules foreach net.add
    net
  }

  def apply(): JtmsGreedy = new JtmsGreedy(TruthMaintenanceNetwork())

}

class JtmsGreedy(val network: TruthMaintenanceNetwork, val random: Random = new Random()) extends JtmsUpdateAlgorithmAbstraction(network, random) {

  var prevModel = network.inAtoms

  //

  var doSelfSupportCheck = false
  var doConsistencyCheck = false //detect wrong computation of odd loop, report inconsistency

  //for inspection:
  var doJtmsSemanticsCheck = false
  //for debugging
  var shuffle = true

  override def update(atoms: Set[Atom]) {

    prevModel = network.inAtoms

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom, Status, String)]()

    try {
      updateImplementation(atoms)

      checkJtmsSemantics()
      checkSelfSupport()
      checkConsistency()
    } catch {
      case e: IncrementalUpdateFailureException => {
        invalidateModel()
      }
      case e: NoSuchElementException => {
        println(e)
      }
    }

  }

  def updateImplementation(atoms: Set[Atom]) {
    atoms foreach setUnknown
    var lastAtom: Option[Atom] = None
    while (network.hasUnknown) {
      network.unknownAtoms foreach findStatus
      val atom = getOptUnknownOtherThan(lastAtom) //ensure that the same atom is not tried consecutively
      if (atom.isDefined) {
        chooseStatusGreedy(atom.get)
      }
      lastAtom = atom
    }
  }

  def getOptUnknownOtherThan(avoid: Option[Atom]): Option[Atom] = {

    if (!prevModel.isEmpty) {
      val a = prevModel.head
      prevModel = prevModel.tail
      return Some(a)
    }

    //TODO improve

    //val atomSet = (network.unknownAtoms diff network.signals) //filter (a => a.predicate.caption == "bit" || a.predicate.caption == "xx1") //TODO
    val atomSet = network.unknownAtoms filter (!_.isInstanceOf[PinnedAtom])

    if (atomSet.isEmpty) return None
    if (atomSet.size == 1) return Some(atomSet.head)

    val atoms = if (shuffle && atomSet.size > 1) (random.shuffle(atomSet.toSeq)) else atomSet

    Some(atoms.head)

    /* stable:

    val atoms = unknownAtoms

    if (atoms.isEmpty) return None
    if (atoms.size == 1) return Some(atoms.head)

    if (doForceChoiceOrder) {
      val maybeAtom: Option[Atom] = forcedChoiceSeq find (status(_) == unknown)
      if (maybeAtom.isDefined) {
        return maybeAtom
      }
    }

    val list = List[Atom]() ++ atoms

    val idx = if (shuffle) { random.nextInt(list.size) } else 0
    val elem = list(idx)

    return Some(elem)
    */

    //prev

    /*
    if (avoid == None) return Some(elem)

    val elemToAvoid = avoid.get
    if (elem != elemToAvoid) return Some(elem)
    return list find (_ != elemToAvoid)
    */
  }

  def chooseStatusGreedy(a: Atom): Unit = {

    if (network.status(a) != unknown)

      if (recordChoiceSeq) choiceSeq = choiceSeq :+ a

    network.justifications(a) find network.posValid match {
      case Some(rule) => chooseIn(rule)
      case None => chooseOut(a)
    }

    network.unknownCons(a) foreach findStatus
  }

  def chooseIn(rulePosValid: NormalRule): Unit = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rulePosValid.head, in, "choose")
    setIn(rulePosValid)
    rulePosValid.neg foreach { a =>
      network.status(a) match {
        case `unknown` => chooseOut(a) //fix status of ancestors
        case `in` => throw new IncrementalUpdateFailureException() //odd loop (within rule) detection
        case `out` => //nothing to be done
      }
    }
    /* not that setIn here has to be called first. consider
       a :- not b.
       b :- not a. ,
       where the choice is for status(a)=in. then, this status needs to be available
       when the spoiler for rule b :- not a is sought.
     */
  }

  def chooseOut(atom: Atom): Unit = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (atom, out, "choose")

    //status(a) = out
    //status = status.updated(atom,out)
    network.updateStatus(atom, out)

    val maybeAtoms: Set[Option[Atom]] = network.openJustifications(atom) map { r => (r.pos find (network.status(_) == unknown)) }
    val unknownPosAtoms = (maybeAtoms filter (_.isDefined)) map (_.get)
    unknownPosAtoms foreach chooseOut //fix status of ancestors
    //note that only positive body atoms are used to create a spoilers, since a rule with an empty body
    //where the negative body is out/unknown is
    setOutSupport(atom: Atom)
  }

  //
  //
  //

  def checkJtmsSemantics(): Unit = {
    if (!doJtmsSemanticsCheck) return
    if (network.atomsNeedingSupp exists (network.supp(_).isEmpty)) {
      throw new RuntimeException("model: " + getModel() + "\nno support for atoms " + (network.atomsNeedingSupp filter (network.supp(_).isEmpty)))
    }
  }

  def checkSelfSupport(): Unit = {
    if (!doSelfSupportCheck) return
    if (network.inAtoms exists unfoundedSelfSupport) {
      throw new RuntimeException("model: " + getModel() + "\nself support exists")
    }
  }

  def checkConsistency(): Unit = {
    if (!doConsistencyCheck) return
    if ((network.inAtoms diff network.factAtoms) exists (a => !(network.justifications(a) exists network.valid))) {
      throw new RuntimeException("model: " + getModel() + "\ninconsistent state: in-atom has no valid justification")
    }
    if ((network.outAtoms diff network.factAtoms) exists (a => (network.justifications(a) exists network.valid))) {
      throw new RuntimeException("model: " + getModel() + "\ninconsistent state: out-atom has valid justification")
    }
  }

  def selfSupport(a: Atom): Boolean = network.supp(a) contains a

  def unfoundedSelfSupport(a: Atom): Boolean = {
    if (!selfSupport(a)) return false
    network.justifications(a) filter network.valid exists (r => !(r.pos contains a))
  }

}