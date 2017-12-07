package jtms.networks

import core._
import core.asp.NormalRule
import jtms.Status

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

/**
  * Created by hb on 6/10/16.
  */

object TruthMaintenanceNetwork {
  def apply() = new OptimizedNetwork()
}

trait TruthMaintenanceNetwork {

  var rules: Set[NormalRule] = Set()

  var cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  var supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  var status: Map[Atom, Status] = new HashMap[Atom, Status]

  //needed originally only in backtracking procedure, which is skipped
  //however, should give a benefit in remove, where many cases can be skipped
  var suppRule: Map[Atom, Option[NormalRule]] = new HashMap[Atom, Option[NormalRule]]

  def justifications(a: Atom): Set[NormalRule]

  def facts: Set[NormalRule]

  def factAtoms: Set[Atom] //note the difference to facts, which are rules with empty bodies!

  def allAtoms: Set[Atom]

  def ruleHeads: Set[Atom]

  def atomsNeedingSupp: Set[Atom] //these are intensional atoms

  def underivableAtoms: Set[Atom]

  //def activeRules() = (rules filter (r => r.pos forall (ruleHeads contains _))).toSet

  //def inactiveRules() = (rules filter (r => !(r.pos intersect underivableAtoms).isEmpty)).toSet

  def contradictionAtom(a: Atom): Boolean

  def inAtoms: Set[Atom]

  def outAtoms: Set[Atom]

  def unknownAtoms: Set[Atom]

  def hasUnknown: Boolean

  def inconsistent: Boolean = unknownAtoms.nonEmpty

  //affected(a) = {x ∈ cons(a) | a ∈ supp(x)}
  def affected(a: Atom): Set[Atom]

  def repercussions(a: Atom) = trans(affected, a)

  def antecedents(a: Atom): Set[Atom]

  def foundations(a: Atom) = trans(antecedents, a)

  def ancestors(a: Atom) = trans(supp, a)

  def valid(rule: NormalRule): Boolean

  def invalid(rule: NormalRule): Boolean

  def posValid(rule: NormalRule): Boolean

  def openJustifications(a: Atom): Set[NormalRule]

  def unknownCons(a: Atom): Set[Atom]

  def removeDeprecatedCons(rule: NormalRule)(a: Atom): Unit

  def clearSupport(a: Atom): Unit

  def setInSupport(a: Atom, rule: NormalRule): Unit

  def setOutSupport(a: Atom, atoms: Set[Atom]): Unit

  def addSupport(a: Atom, newAtom: Atom): Unit

  def updateStatus(a: Atom, status: Status): Unit

  def register(rule: NormalRule): Boolean

  def register(a: Atom): Unit

  def deregister(a: Atom): Unit

  def deregister(rule: NormalRule): Boolean

  def trans[T](f: T => Set[T], t: T): Set[T] = {
    trans(f)(f(t))
  }

  @tailrec
  final def trans[T](f: T => Set[T])(s: Set[T]): Set[T] = {
    val next = s.flatMap(f)
    val nextSet = next ++ s
    if (s == nextSet || next.isEmpty) {
      return s
    }
    trans(f)(nextSet)
  }

}
