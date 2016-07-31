package jtms

import core.{ContradictionAtom, Falsum, Atom}
import core.asp.NormalRule

import scala.annotation.tailrec
import scala.collection.mutable.{Map,HashMap,Set}

/**
  * Created by hb on 6/10/16.
  */
trait Jtms {

  var rules: List[NormalRule] = List()

  val cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status]

  val suppRule: Map[Atom, Option[NormalRule]] = new HashMap[Atom,Option[NormalRule]]

  def add(rule: NormalRule)
  def remove(rule: NormalRule)
  def recompute()
  def getModel(): Option[collection.immutable.Set[Atom]]
  def set(model: collection.immutable.Set[Atom]): Boolean

  //

  //book keeping:
  var recordStatusSeq = true
  var statusSeq = Seq[(Atom,Status,String)]()

  var recordChoiceSeq = true
  var choiceSeq = Seq[Atom]()

  def justifications(a: Atom) = rules filter (_.head == a)

  def allAtoms() = (rules flatMap (_.atoms)) toSet

  def facts() = rules filter (_.isFact) map (_.head) toSet

  def ruleHeads() = rules map (_.head) toSet

  def atomsNeedingSupp() = ruleHeads diff facts

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
