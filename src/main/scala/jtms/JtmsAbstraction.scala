package jtms

import core.Atom
import core.asp.NormalRule

import scala.util.Random

/**
  * Created by hb on 6/10/16.
  */
 class JtmsAbstraction(random: Random = new Random()) extends Jtms {

  //def isChoiceAtom(atom: Atom) = atom.predicate.toString == "bit" //TODO

  //def unknownChoiceAtoms() = unknownAtoms() filter isChoiceAtom

  //

  override def allAtoms() = __allAtoms

  var __allAtoms: Set[Atom] = Set()

  var __choiceAtoms: Set[Atom] = Set()

  override def justifications(a: Atom) = __justifications(a)

  var __justifications: Map[Atom, Set[NormalRule]] = Map.empty.withDefaultValue(Set())

  var __rulesAtomsOccursIn: Map[Atom, Set[NormalRule]] = Map.empty.withDefaultValue(Set())

  var __atomsWithStatus: Map[Status, Set[Atom]] = Map.empty.withDefaultValue(Set())
  var __stateHash: Long = IncrementalHashCode.emptyHash

  var __lightweightStateHash: Long = IncrementalHashCode.emptyHash

  //atoms from streaming; specific logic beyond tms
  var __signals: Set[Atom] = Set()

  var __suppHash: Map[Atom, Long] = Map()

  override def inAtoms() = __atomsWithStatus(in)

  override def outAtoms(): Set[Atom] = __atomsWithStatus(out)

  override def unknownAtoms(): Set[Atom] = __atomsWithStatus(unknown)

  override def hasUnknown(): Boolean = unknownAtoms().nonEmpty

  override def signals(): Set[Atom] = __signals

  //return true iff rule is new
  def register(rule: NormalRule): Boolean = {
    if (rules contains rule) return false //list representation!

    rules = rules + rule

    /*
    __ruleHeads = __ruleHeads + rule.head
    if (rule.body.isEmpty) __factAtoms = __factAtoms + rule.head
    */

    __justifications = __justifications updated(rule.head, __justifications(rule.head) + rule)

    val ruleOccurrences = rule.atoms map (a => (a, __rulesAtomsOccursIn(a) + rule))
    __rulesAtomsOccursIn = __rulesAtomsOccursIn ++ ruleOccurrences

    rule.atoms foreach register
    //rule.body foreach (cons(_) += rule.head)
    rule.body foreach { atom =>
      cons = cons updated(atom, cons(atom) + rule.head)
    }
    true
  }

  def register(a: Atom) {
    if (!status.isDefinedAt(a)) {
      //use this immediately as test whether the atom exists; all atoms need to have a status
// TODO: do we need this?      if (recordStatusSeq) statusSeq = statusSeq :+ (a, out, "register")

      __allAtoms = __allAtoms + a

      status = status.updated(a, out)

      //if (isChoiceAtom(a)) {
      //  __choiceAtoms = __choiceAtoms + a
      //  __lightweightStateHash = IncrementalHashCode.addHashCode(__lightweightStateHash, (a, out))
      //} else
      if (isSignal(a)) {
        __signals = __signals + a
      } else {
        __stateHash = IncrementalHashCode.addHashCode(__stateHash, (a, out))
      }

      __atomsWithStatus = __atomsWithStatus.updated(out, __atomsWithStatus(out) + a)

      cons = cons.updated(a, Set[Atom]())
      clearSupport(a)
      suppRule = suppRule.updated(a, None)
      //      status(a) = out
      //      cons(a) = Set[Atom]()
      //      supp(a) = Set[Atom]()
      //      suppRule(a) = None
    }
  }

  def __updateStatus(a: Atom, newStatus: Status): Unit = {

    var oldStatus = status(a)

    if (oldStatus != newStatus) {
      if (!signals().contains(a)) {
        if (oldStatus == in || oldStatus == out) {
          __stateHash = IncrementalHashCode.removeHashCode(__stateHash, (a, oldStatus))
//          if (isChoiceAtom(a)) {
//            __lightweightStateHash = IncrementalHashCode.removeHashCode(__lightweightStateHash, (a, oldStatus))
//          }
        }
        if (newStatus == in || newStatus == out) {
          __stateHash = IncrementalHashCode.addHashCode(__stateHash, (a, newStatus))
//          if (isChoiceAtom(a)) {
//            __lightweightStateHash = IncrementalHashCode.addHashCode(__lightweightStateHash, (a, newStatus))
//          }
        }
      }
      status = status.updated(a, newStatus)
      __atomsWithStatus = __atomsWithStatus.updated(newStatus, __atomsWithStatus(newStatus) + a)
      __atomsWithStatus = __atomsWithStatus.updated(oldStatus, __atomsWithStatus(oldStatus) - a)
    }

  }


  //
  // remove
  //

  //return true iff rules was present (and deleted)
  def unregister(rule: NormalRule): Boolean = {
    if (!(rules contains rule)) return false

    rules = rules - rule

    __justifications = __justifications updated(rule.head, __justifications(rule.head) filter (_ != rule))

    val ruleOccurrences = rule.atoms map (a => (a, __rulesAtomsOccursIn(a) - rule))
    __rulesAtomsOccursIn = __rulesAtomsOccursIn ++ ruleOccurrences

    val atomsToBeRemoved = rule.atoms filter (a => __rulesAtomsOccursIn(a).isEmpty)
    val remainingAtoms = __allAtoms diff atomsToBeRemoved

    __cleanupSupportingData()

    atomsToBeRemoved foreach unregister
    (rule.body intersect remainingAtoms) foreach removeDeprecatedCons(rule)

    true
  }

  def removeDeprecatedCons(rule: NormalRule)(a: Atom): Unit = {
    if (!(justifications(rule.head) exists (_.body contains a))) {
      //cons(a) -= rule.head
      cons = cons.updated(a, cons(a) - rule.head)
    }
  }

  def unregister(a: Atom): Unit = {
    __allAtoms = __allAtoms - a

    val oldStatus = status(a)

//    if (isChoiceAtom(a)) {
//      __choiceAtoms = __choiceAtoms - a
//      if (oldStatus == in || oldStatus == out) {
//        __lightweightStateHash = IncrementalHashCode.removeHashCode(__lightweightStateHash,(a, oldStatus))
//      }
//    }

    __atomsWithStatus = __atomsWithStatus.updated(oldStatus, __atomsWithStatus(oldStatus) - a)

    if (!signals().contains(a) && (oldStatus==in ||oldStatus==out)) {
      __stateHash = IncrementalHashCode.removeHashCode(__stateHash,(a, oldStatus))
    }

    status = status - a
    cons = cons - a
    supp = supp - a
    __suppHash = __suppHash - a
    suppRule = suppRule - a

    if (isSignal(a))
      __signals = __signals - a
    //    status remove a
    //    cons remove a
    //    supp remove a
    //    suppRule remove a
  }

  var __cleanup = 0;

  def __cleanupSupportingData(force: Boolean = false): Unit = {
    __cleanup = __cleanup + 1
    if (__cleanup % 1000 == 0 || force) {
      __cleanup = 0

      __justifications = __justifications filter { case (atom,rules) => rules.nonEmpty }
      __rulesAtomsOccursIn = __rulesAtomsOccursIn filter { case (atom,rules) => rules.nonEmpty }
    }
  }

  //
  // set
  //



  def clearSupport(a:Atom): Unit ={
    supp = supp.updated(a, Set())
    __suppHash = __suppHash.updated(a, IncrementalHashCode.emptyHash)
  }
  def setSupport(a: Atom, atoms: Set[Atom]): Unit = {
    supp = supp.updated(a, atoms)
    __suppHash = __suppHash.updated(a, IncrementalHashCode.hash(atoms diff signals))
  }

  def addSupport(a:Atom, newAtom:Atom): Unit ={
    supp = supp.updated(a, supp(a) + newAtom)
    if(!signals().contains(newAtom))
      __suppHash = __suppHash.updated(a, IncrementalHashCode.addHashCode(__suppHash(a),newAtom))
  }

}

object IncrementalHashCode {

  val emptyHash: Long = Set().hashCode()

  def hash(atoms: Set[Atom]) = atoms.foldLeft(emptyHash)(addHashCode)

  def addHashCode(hash: Long, item: Any): Long = {

    val newHash = (hash ) + item.hashCode()

    newHash
  }

  def removeHashCode(hash: Long, element: Any): Long = {
//    val newHash = (hash - element.hashCode()) / 16777619
    val newHash = hash - element.hashCode()
    newHash
  }
}
