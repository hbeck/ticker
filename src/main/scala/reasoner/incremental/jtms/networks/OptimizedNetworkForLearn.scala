package reasoner.incremental.jtms.networks

import core.{Atom, PinnedAtom}
import core.asp.NormalRule
import reasoner.incremental.jtms._


/**
  * Created by hb on 05.04.17.
  */
class OptimizedNetworkForLearn extends OptimizedNetwork {

  //var __lightweightStateHash: Long = IncrementalHashCode.emptyHash

  var __stateHash: Long = IncrementalHashCode.emptyHash
  var __suppHash: Map[Atom, Long] = Map()
  //atoms from streaming; specific logic beyond tms
  var __signals: Set[Atom] = Set()

  def dataIndependentRules: Set[NormalRule] = rules filter dataIndependentRule

  def dataIndependentRule(r: NormalRule): Boolean = {
    !r.atoms.exists(pinned(_)) //include testing head for facts
  }

  def signals = allAtoms filter isSignal

  //note: this is implementation specific due to use case
  //(PinnedAtoms obtained after grounding, and only signals (i.e., stream atoms) are pinned)
  def isSignal(a: Atom) = pinned(a)

  def pinned(atom: Atom) = atom.isInstanceOf[PinnedAtom]


  override def register(a: Atom) {
    if (!status.isDefinedAt(a)) {

      __allAtoms = __allAtoms + a

      status = status.updated(a, out)

      if (isSignal(a)) {
        __signals = __signals + a
      } else {
        __stateHash = IncrementalHashCode.addHashCode(__stateHash, (a, out))
      }

      __atomsWithStatus = __atomsWithStatus.updated(out, __atomsWithStatus(out) + a)

      cons = cons.updated(a, Set[Atom]())
      clearSupport(a)
    }
  }

  override def updateStatus(a: Atom, newStatus: Status): Unit = {

    val oldStatus = status(a)

    if (oldStatus != newStatus) {
      if (!signals.contains(a)) {
        if (oldStatus == in || oldStatus == out) {
          __stateHash = IncrementalHashCode.removeHashCode(__stateHash, (a, oldStatus))
        }
        if (newStatus == in || newStatus == out) {
          __stateHash = IncrementalHashCode.addHashCode(__stateHash, (a, newStatus))
        }
      }
      status = status.updated(a, newStatus)
      __atomsWithStatus = __atomsWithStatus.updated(newStatus, __atomsWithStatus(newStatus) + a)
      __atomsWithStatus = __atomsWithStatus.updated(oldStatus, __atomsWithStatus(oldStatus) - a)
    }
  }

  override def deregister(a: Atom): Unit = {
    __allAtoms = __allAtoms - a

    val oldStatus = status(a)

    __atomsWithStatus = __atomsWithStatus.updated(oldStatus, __atomsWithStatus(oldStatus) - a)

    if (!signals.contains(a) && (oldStatus == in || oldStatus == out)) {
      __stateHash = IncrementalHashCode.removeHashCode(__stateHash, (a, oldStatus))
    }

    status = status - a
    cons = cons - a
    supp = supp - a
    __suppHash = __suppHash - a
    suppRule = suppRule - a

    if (isSignal(a))
      __signals = __signals - a
  }

  override def clearSupport(a: Atom): Unit = {
    super.clearSupport(a)
    __suppHash = __suppHash.updated(a, IncrementalHashCode.emptyHash)
  }

  override def setOutSupport(a: Atom, atoms: Set[Atom]): Unit = {
    super.setOutSupport(a,atoms)
    __suppHash = __suppHash.updated(a, IncrementalHashCode.hash(atoms diff signals))
  }

  override def setInSupport(a: Atom, rule: NormalRule): Unit = {
    super.setInSupport(a,rule)
    __suppHash = __suppHash.updated(a, IncrementalHashCode.hash(rule.body diff signals))
  }

  override def addSupport(a: Atom, newAtom: Atom): Unit = {
    super.addSupport(a,newAtom)
    if (!signals.contains(newAtom)) {
      __suppHash = __suppHash.updated(a, IncrementalHashCode.addHashCode(__suppHash(a), newAtom))
    }
  }

}

object IncrementalHashCode {

  val emptyHash: Long = Set().hashCode()

  def hash(atoms: Set[Atom]) = atoms.foldLeft(emptyHash)(addHashCode)

  def addHashCode(hash: Long, item: Any): Long = hash + item.hashCode()

  def removeHashCode(hash: Long, element: Any): Long = hash - element.hashCode()

}

