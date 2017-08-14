package jtms.networks

import core.{Atom, ContradictionAtom, Falsum}
import core.asp.NormalRule
import jtms._

/**
  * Created by hb on 6/10/16.
  */
class OptimizedNetwork extends TruthMaintenanceNetwork {

  val supportCleanupThreshold = 5000
  var __justifications: Map[Atom, Set[NormalRule]] = Map.empty.withDefaultValue(Set())

  var __rulesAtomsOccursIn: Map[Atom, Set[NormalRule]] = Map.empty.withDefaultValue(Set())

  var __atomsWithStatus: Map[Status, Set[Atom]] = Map.empty.withDefaultValue(Set())

  var __allAtoms: Set[Atom] = Set()

  var __choiceAtoms: Set[Atom] = Set()

  def allAtoms() = __allAtoms

  def justifications(a: Atom) = __justifications(a)

  def inAtoms = __atomsWithStatus(in)

  def outAtoms: Set[Atom] = __atomsWithStatus(out)

  def unknownAtoms: Set[Atom] = __atomsWithStatus(unknown)

  def hasUnknown: Boolean = unknownAtoms.nonEmpty

  // re-implementation of SimpleNetwork definition.
  // Some implementations could be optimized, but they are currently not a bottleneck!

  def facts: Set[NormalRule] = rules filter (_.isFact)

  def factAtoms: Set[Atom] = facts map (_.head)

  def ruleHeads = rules map (_.head)

  def atomsNeedingSupp: Set[Atom] = ruleHeads diff factAtoms

  def underivableAtoms = allAtoms diff ruleHeads

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom] || a == Falsum

  def affected(a: Atom): Set[Atom] = cons(a) filter (supp(_) contains a)

  def antecedents(a: Atom): Set[Atom] = if (status(a) == in)
    supp(a)
  else
    Set()

  def valid(rule: NormalRule): Boolean =
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))

  def invalid(rule: NormalRule): Boolean =
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))

  def posValid(rule: NormalRule): Boolean =
    (rule.pos forall (status(_) == in)) && (!(rule.neg exists (status(_) == in)))

  def openJustifications(a: Atom): Set[NormalRule] = justifications(a) filter (!invalid(_))

  def unknownCons(a: Atom): Set[Atom] = cons(a) filter (status(_) == unknown)

  def removeDeprecatedCons(rule: NormalRule)(a: Atom): Unit = {
    if (!(justifications(rule.head) exists (_.body contains a))) {
      cons = cons.updated(a, cons(a) - rule.head)
    }
  }

  // re-implementation of SimpleNetwork definition.
  // Some implementations could be optimized, but they are currently not a bottleneck!


  //return true iff rule is new
  def register(rule: NormalRule): Boolean = {
    if (rules contains rule) return false //list representation!

    rules = rules + rule

    __justifications = __justifications updated(rule.head, __justifications(rule.head) + rule)

    val ruleOccurrences = (rule.atoms toSeq) map (a => (a, __rulesAtomsOccursIn(a) + rule))
    __rulesAtomsOccursIn = __rulesAtomsOccursIn ++ ruleOccurrences

    rule.atoms foreach register
    rule.body foreach { atom =>
      cons = cons updated(atom, cons(atom) + rule.head)
    }

    true
  }

  def register(a: Atom) {
    if (!status.isDefinedAt(a)) {
      __allAtoms = __allAtoms + a
      status = status.updated(a, out)
      __atomsWithStatus = __atomsWithStatus.updated(out, __atomsWithStatus(out) + a)
      cons = cons.updated(a, Set[Atom]())
      clearSupport(a)
    }
  }

  def updateStatus(a: Atom, newStatus: Status): Unit = {
    val oldStatus = status(a)
    if (oldStatus != newStatus) {
      status = status.updated(a, newStatus)
      __atomsWithStatus = __atomsWithStatus.updated(newStatus, __atomsWithStatus(newStatus) + a)
      __atomsWithStatus = __atomsWithStatus.updated(oldStatus, __atomsWithStatus(oldStatus) - a)
    }
  }

  //
  // remove
  //

  //return true iff rules was present (and deleted)
  def deregister(rule: NormalRule): Boolean = {
    if (!(rules contains rule)) return false

    rules = rules - rule

    __justifications = __justifications updated(rule.head, __justifications(rule.head) - rule)


    // not using set-semantics of atoms speeds up computation
    val ruleOccurrences = (rule.atoms toSeq) map (a => (a, __rulesAtomsOccursIn(a) - rule))
    __rulesAtomsOccursIn = __rulesAtomsOccursIn ++ ruleOccurrences

    // note: here we should use set semantics - its faster
    val atomsToBeRemoved = rule.atoms filter (a => __rulesAtomsOccursIn(a).isEmpty)
    val remainingAtoms = __allAtoms diff atomsToBeRemoved

    __cleanupSupportingData()

    atomsToBeRemoved foreach deregister
    (rule.body intersect remainingAtoms) foreach removeDeprecatedCons(rule)

    true
  }

  def deregister(a: Atom): Unit = {

    __allAtoms = __allAtoms - a

    val oldStatus = status(a)

    __atomsWithStatus = __atomsWithStatus.updated(oldStatus, __atomsWithStatus(oldStatus) - a)

    status = status - a
    cons = cons - a
    supp = supp - a
    suppRule = suppRule - a

  }

  var __cleanup = 0;

  def __cleanupSupportingData(force: Boolean = false): Unit = {
    __cleanup = __cleanup + 1
    if (__cleanup % supportCleanupThreshold == 0 || force) {
      __cleanup = 0


      __justifications = __justifications filter { case (atom, r) => r.nonEmpty }
      __rulesAtomsOccursIn = __rulesAtomsOccursIn filter { case (atom, r) => r.nonEmpty }
    }
  }

  //
  // set
  //


  def clearSupport(a: Atom): Unit = {
    supp = supp.updated(a, Set())
    suppRule = suppRule.updated(a, None)
  }

  def setOutSupport(a: Atom, atoms: Set[Atom]): Unit = {
    supp = supp.updated(a, atoms)
    suppRule = suppRule.updated(a, None)
  }

  def setInSupport(a: Atom, rule: NormalRule): Unit = {
    suppRule = suppRule.updated(rule.head, Some(rule))
    supp = supp.updated(a, rule.body)
  }

  def addSupport(a: Atom, newAtom: Atom): Unit = {
    supp = supp.updated(a, supp(a) + newAtom)
  }

}

