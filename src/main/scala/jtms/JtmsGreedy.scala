package jtms

import core._
import core.asp.{NormalProgram, NormalRule}

import scala.collection.mutable.Set
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
  var doTmsSemanticsCheck = true //introduced while debugging remove problems
  var shuffle = true

  override def update(atoms: Set[Atom]) {

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom,Status,String)]()

    try {
      updateStepwise(atoms)

      checkTmsSemantics()
      checkSelfSupport()
      checkConsistency()
    } catch {
      case e:IncrementalUpdateFailureException => {
        invalidateModel()
      }
    }

  }

  def updateStepwise(atoms: Set[Atom]) {
    atoms foreach setUnknown
    var lastAtom: Option[Atom] = None
    while (hasUnknown) {
      unknownAtoms foreach determineAndPropagateStatus
      val atom = getOptUnknownOtherThan(lastAtom) //ensure that the same atom is not tried consecutively
      if (atom.isDefined) {
        fixAndDetermineAndPropagateStatus(atom.get)
      }
      lastAtom = atom
    }
  }

  def getOptUnknownOtherThan(atom: Option[Atom]): Option[Atom] = {

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

    if (atom == None) return Some(elem)
    val elemToAvoid = atom.get
    if (elem != elemToAvoid) return Some(elem)
    return list find (_ != elemToAvoid)
  }

  //add book keeping
  override def setIn(rule: NormalRule) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rule.head,in,"set")
    super.setIn(rule)
  }

  //add book keeping
  override def setOut(a: Atom) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"set")
    super.setOut(a)
  }

  def fixAndDetermineAndPropagateStatus(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (recordChoiceSeq) choiceSeq = choiceSeq :+ a

    justifications(a) find posValid match {
      case Some(rule) => fixIn(rule)
      case None => fixOut(a)
    }

    unknownCons(a) foreach determineAndPropagateStatus
  }

  def fixIn(rulePosValid: NormalRule): Unit = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rulePosValid.head, in,"fix")
    setIn(rulePosValid)
    rulePosValid.neg foreach { a =>
      status(a) match {
        case `unknown` => fixOut(a) //fix ancestors
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

  def fixOut(a: Atom): Unit = {
    status(a) = out
    if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"fix")

    val maybeAtoms: List[Option[Atom]] = openJustifications(a) map { r => (r.pos find (status(_)==unknown)) }
    val unknownPosAtoms = (maybeAtoms filter (_.isDefined)) map (_.get)
    unknownPosAtoms foreach fixOut //fix ancestors
    //note that only positive body atoms are used to create a spoilers, since a rule with an empty body
    //where the negative body is out/unknown is
    setOutSupport(a: Atom)
  }

  //
  //
  //

  def checkTmsSemantics(): Unit = {
    if (!doTmsSemanticsCheck) return
    if (atomsNeedingSupp exists (supp(_).isEmpty)) {
      throw new RuntimeException("no support for atoms "+(atomsNeedingSupp filter (supp(_).isEmpty)))
    }
  }

  def checkSelfSupport(): Unit = {
    if (!doSelfSupportCheck) return
    if (inAtoms exists unfoundedSelfSupport) {
      throw new RuntimeException("self support")
    }
  }

  def checkConsistency(): Unit = {
    if (!doConsistencyCheck) return
    if ((inAtoms diff facts) exists (a => !(justifications(a) exists valid))) {
      throw new RuntimeException("inconsistent state: in atom has no valid justification")
    }
    if ((outAtoms diff facts) exists (a => (justifications(a) exists valid))) {
      throw new RuntimeException("inconsistent state: out atom has valid justification")
    }
  }

  def selfSupport(a:Atom): Boolean = supp(a) contains a

  def unfoundedSelfSupport(a: Atom): Boolean = {
    if (!selfSupport(a)) return false
    justifications(a) filter valid exists (r => !(r.pos contains a))
  }

}