package jtms

import core.Atom
import core.asp.NormalRule

/**
  * Created by FM on 29.10.16.
  */
class SimpleJtms extends Jtms {
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

  override def updateStatus(a: Atom, newStatus: Status): Unit = status = status.updated(a, newStatus)

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

  override def unregister(a: Atom) = {
    status = status - a
    cons = cons - a
    supp = supp - a
  }

  override def unregister(rule: NormalRule): Boolean = {
    if (!(rules contains rule)) return false

    rules = rules - rule

    val remainingAtoms = rules flatMap (_.atoms)

    (rule.atoms diff remainingAtoms) foreach unregister
    (rule.body intersect remainingAtoms) foreach removeDeprecatedCons(rule)

    true
  }
}
