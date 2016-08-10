package jtms

import core._
import core.asp.{NormalProgram, NormalRule}

import scala.util.Random

object JtmsGreedy {

  def apply(P: NormalProgram): JtmsGreedy = {
    val net = new JtmsGreedy()
    P.rules foreach net.add
    net
  }

}

case class JtmsGreedy(random: Random = new Random()) extends JtmsAbstraction {

  var doSelfSupportCheck = true
  var doConsistencyCheck = true //detect wrong computation of odd loop, report inconsistency

  //for inspection:
  var doJtmsSemanticsCheck = true //for debugging
  var shuffle = true

  override def update(atoms: Predef.Set[Atom]) {

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom,Status,String)]()

    try {
      updateGreedy(atoms)

      checkJtmsSemantics()
      checkSelfSupport()
      checkConsistency()
    } catch {
      case e:IncrementalUpdateFailureException => {
        invalidateModel()
      }
    }

  }

  def updateGreedy(atoms: Predef.Set[Atom]) {
    atoms foreach setUnknown
    var lastAtom: Option[Atom] = None
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      val atom = getOptUnknownOtherThan(lastAtom) //ensure that the same atom is not tried consecutively
      if (atom.isDefined) {
        chooseStatusGreedy(atom.get)
      }
      lastAtom = atom
    }
  }

  def getOptUnknownOtherThan(a: Option[Atom]): Option[Atom] = { //TODO improve

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
    val idx = if (shuffle) { util.Random.nextInt(list.size) } else 0
    val elem = list(idx)

    if (a == None) return Some(elem)
    val elemToAvoid = a.get
    if (elem != elemToAvoid) return Some(elem)
    return list find (_ != elemToAvoid)
  }

  def chooseStatusGreedy(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (recordChoiceSeq) choiceSeq = choiceSeq :+ a

    justifications(a) find posValid match {
      case Some(rule) => chooseIn(rule)
      case None => chooseOut(a)
    }

    unknownCons(a) foreach findStatus
  }

  def chooseIn(rulePosValid: NormalRule): Unit = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rulePosValid.head, in,"choose")
    setIn(rulePosValid)
    rulePosValid.neg foreach { a =>
      status(a) match {
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
    //status(a) = out
    status = status.updated(atom,out)
    if (recordStatusSeq) statusSeq = statusSeq :+ (atom,out,"choose")

    val maybeAtoms: Seq[Option[Atom]] = openJustifications(atom) map { r => (r.pos find (status(_)==unknown)) }
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
    if (atomsNeedingSupp exists (supp(_).isEmpty)) {
      throw new RuntimeException("model: "+getModel()+"\nno support for atoms "+(atomsNeedingSupp filter (supp(_).isEmpty)))
    }
  }

  def checkSelfSupport(): Unit = {
    if (!doSelfSupportCheck) return
    if (inAtoms exists unfoundedSelfSupport) {
      throw new RuntimeException("model: "+getModel()+"\nself support exists")
    }
  }

  def checkConsistency(): Unit = {
    if (!doConsistencyCheck) return
    if ((inAtoms diff factAtoms) exists (a => !(justifications(a) exists valid))) {
      throw new RuntimeException("model: "+getModel()+"\ninconsistent state: in-atom has no valid justification")
    }
    if ((outAtoms diff factAtoms) exists (a => (justifications(a) exists valid))) {
      throw new RuntimeException("model: "+getModel()+"\ninconsistent state: out-atom has valid justification")
    }
  }

  def selfSupport(a:Atom): Boolean = supp(a) contains a

  def unfoundedSelfSupport(a: Atom): Boolean = {
    if (!selfSupport(a)) return false
    justifications(a) filter valid exists (r => !(r.pos contains a))
  }

}