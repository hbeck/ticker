package jtms

import core._
import core.asp.NormalRule

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

/**
  * Created by hb on 6/10/16.
  */
trait Jtms {

  var rules: Set[NormalRule] = Set()

  var cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  var supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  var status: Map[Atom, Status] = new HashMap[Atom, Status]

  //needed originally only in backtracking procedure, which is skipped
  //however, should give a benefit in remove, where many cases can be skipped
  var suppRule: Map[Atom, Option[NormalRule]] = new HashMap[Atom,Option[NormalRule]]

  def add(rule: NormalRule)
  def remove(rule: NormalRule)
  //def recompute() //TODO
  def getModel(): Option[collection.immutable.Set[Atom]]
  def set(model: collection.immutable.Set[Atom]): Boolean

  //

  //book keeping:
  var recordStatusSeq = true
  var statusSeq = Seq[(Atom,Status,String)]()

  var recordChoiceSeq = true
  var choiceSeq = Seq[Atom]()

  def justifications(a: Atom): Set[NormalRule] = rules filter (_.head == a)

  def facts() = rules filter (_.isFact) toSet

  def factAtoms() = facts map (_.head) //note the difference to facts, which are rules with empty bodies!

  def allAtoms(): Predef.Set[Atom] = (rules flatMap (_.atoms)) toSet

  def ruleHeads() = rules map (_.head) toSet

  def atomsNeedingSupp() = ruleHeads diff factAtoms

  def underivableAtoms() = allAtoms diff ruleHeads

  def dataIndependentRules(): Set[NormalRule] = rules filter dataIndependentRule

  def dataIndependentRule(r: NormalRule): Boolean = {
    ! r.atoms.exists(pinned(_)) //include testing head for facts
  }

  def extensionalAtoms() = allAtoms filter extensional

  //note: this is implementation specific due to use case
  //(PinnedAtoms obtained after grounding, and only extensional atoms are pinned)
  def extensional(a: Atom) = pinned(a)

  def pinned(atom: Atom) = atom.isInstanceOf[PinnedAtom]

  //def activeRules() = (rules filter (r => r.pos forall (ruleHeads contains _))).toSet

  //def inactiveRules() = (rules filter (r => !(r.pos intersect underivableAtoms).isEmpty)).toSet

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom] || a == Falsum

  def inAtoms() = allAtoms filter (status(_) == in)
  def outAtoms() = allAtoms filter (status(_) == out)
  def unknownAtoms() = allAtoms filter (status(_) == unknown)

  def hasUnknown = allAtoms exists (status(_) == unknown)

  //affected(a) = {x ∈ cons(a) | a ∈ supp(x)}
  def affected(a: Atom): Set[Atom] = cons(a) filter (supp(_) contains a)

  def repercussions(a: Atom) = trans(affected, a)

  def antecedents(a: Atom): Set[Atom] = {
    if (status(a) == in) return supp(a)
    Set()
  }

  def foundations(a: Atom) = trans(antecedents, a)

  def ancestors(a: Atom) = trans(supp, a)

  def valid(rule: NormalRule) =
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))

  def invalid(rule: NormalRule) =
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))

  def posValid(rule: NormalRule) =
    (rule.pos forall (status(_) == in)) && (!(rule.neg exists (status(_) == in)))

  def openJustifications(a: Atom) = justifications(a) filter (!invalid(_))

  def unknownCons(a: Atom) = cons(a) filter (status(_) == unknown)

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
