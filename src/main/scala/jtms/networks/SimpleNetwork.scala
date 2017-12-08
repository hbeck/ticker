package jtms.networks

import core.{Atom, ContradictionAtom, Falsum}
import core.asp.NormalRule
import jtms._

/**
  * Created by FM on 29.10.16.
  */
class SimpleNetwork extends TruthMaintenanceNetwork {
  def justifications(a: Atom): Set[NormalRule] = rules filter (_.head == a)

  def facts: Set[NormalRule] = rules filter (_.isFact)

  def factAtoms: Set[Atom] = facts map (_.head)

  def allAtoms: Set[Atom] = rules flatMap (_.atoms)

  def ruleHeads = rules map (_.head)

  def atomsNeedingSupp: Set[Atom] = ruleHeads diff factAtoms

  def underivableAtoms = allAtoms diff ruleHeads

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom] || a == Falsum

  def inAtoms: Set[Atom] = allAtoms filter (status(_) == in)

  def outAtoms: Set[Atom] = allAtoms filter (status(_) == out)

  def unknownAtoms: Set[Atom] = allAtoms filter (status(_) == unknown)

  def hasUnknown: Boolean = allAtoms exists (status(_) == unknown)
  def affected(a: Atom): Set[Atom] = cons(a) filter (supp(_) contains a)

  def antecedents(a: Atom): Set[Atom] = {
    if (status(a) == in) return supp(a)
    Set()
  }

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


  override def clearSupport(a: Atom): Unit = {
    supp = supp.updated(a, Set())
    suppRule = suppRule.updated(a, None)
  }

  override def setOutSupport(a: Atom, atoms: Set[Atom]): Unit = {
    supp = supp.updated(a, atoms)
    suppRule = suppRule.updated(a, None)
  }

  override def setInSupport(a: Atom, rule: NormalRule): Unit = {
    suppRule = suppRule.updated(rule.head, Some(rule))
    supp = supp.updated(a, rule.body)
  }

  override def addSupport(a: Atom, newAtom: Atom): Unit = {
    supp = supp.updated(a, supp(a) + newAtom)
  }

  override def updateStatus(a: Atom, newStatus: Status): Unit = {
    status = status.updated(a, newStatus)
  }

  override def register(rule: NormalRule): Boolean = {
    if (rules contains rule) return false //list representation!

    rules = rules + rule

    rule.atoms foreach register
    rule.body foreach { atom =>
      cons = cons updated(atom, cons(atom) + rule.head)
    }

    true
  }

  override def register(a: Atom) = {
    if (!status.isDefinedAt(a)) {
      status = status.updated(a, out)
      cons = cons.updated(a, Set[Atom]())
      clearSupport(a)
    }
  }

  override def deregister(a: Atom) = {
    status = status - a
    cons = cons - a
    supp = supp - a
  }

  override def deregister(rule: NormalRule): Boolean = {
    if (!(rules contains rule)) return false

    rules = rules - rule

    val remainingAtoms = rules flatMap (_.atoms)

    (rule.atoms diff remainingAtoms) foreach deregister
    (rule.body intersect remainingAtoms) foreach removeDeprecatedCons(rule)

    true
  }
}
