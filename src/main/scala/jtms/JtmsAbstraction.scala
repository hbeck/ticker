package jtms

import core.Atom
import core.asp.NormalRule

import scala.collection.mutable.Set
import scala.util.Random

/**
  * Created by hb on 6/10/16.
  */
abstract class JtmsAbstraction(random: Random = new Random()) extends Jtms with ChoiceControl {

  override def allAtoms() = _atomsCache.to
  var _atomsCache: Set[Atom]= Set()

  def update(atoms: Set[Atom])

  //based on JTMS update algorithm
  override def add(rule: NormalRule): Unit = {
    register(rule)
    if (status(rule.head) == in) return
    if (invalid(rule)) { supp(rule.head) += findSpoiler(rule).get; return }
    val atoms = repercussions(rule.head) + rule.head
    update(atoms)
  }

  override def remove(rule: NormalRule): Unit = {
    unregister(rule)
    if (!(allAtoms contains rule.head)) return
    if (status(rule.head) == out) return
    //if (suppRule(rule.head).isDefined && suppRule(rule.head).get != rule) return //.isDefined needed if previous state was inconsistent
    val atoms = repercussions(rule.head) + rule.head
    update(atoms)
  }

  override def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None //not dealt with; left for old test-cases
    if (hasUnknown) return None
    Some(atoms.toSet)
  }

  //
  //  update sub-procedures
  //

  def findStatus(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (validation(a) || invalidation(a))
      unknownCons(a) foreach findStatus
  }

  def validation(a: Atom): Boolean = {
    justifications(a) find valid match {
      case Some(rule) => setIn(rule); true
      case None => false
    }
  }

  def invalidation(a: Atom): Boolean = {
    if (justifications(a) forall invalid) {
      setOut(a)
      return true
    }
    false
  }

  def register(rule: NormalRule): Unit = {
    if (rules contains rule) return //list representation!
    rules = rules :+ rule
    rule.atoms foreach register
    rule.body foreach (cons(_) += rule.head)
  }

  def register(a: Atom) {
    if (!status.isDefinedAt(a)) { status(a) = out
      if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"register")
    }
    if (!cons.isDefinedAt(a)) cons(a) = Set[Atom]()
    if (!supp.isDefinedAt(a)) supp(a) = Set[Atom]()
    _atomsCache add a
    //if (!suppRule.isDefinedAt(a)) suppRule(a) = None
  }

  def invalidateModel(): Unit = {
    atomsNeedingSupp foreach setUnknown
  }

  def setIn(rule: NormalRule) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rule.head,in,"set")
    status(rule.head) = in
    supp(rule.head) = Set() ++ rule.body
    //suppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"set")
    status(a) = out
    setOutSupport(a)
    //suppRule(a) = None
  }

  def setUnknown(a: Atom) = {
    status(a) = unknown
    supp(a) = Set()
    //suppRule(a) = None
  }

  def findSpoiler(rule: NormalRule): Option[Atom] = {
    if (random.nextDouble() < 0.5) {
      rule.pos find (status(_) == out) match {
        case None => rule.neg find (status(_) == in)
        case opt => opt
      }
    } else {
      rule.neg find (status(_) == in) match {
        case None => rule.pos find (status(_) == out)
        case opt => opt
      }
    }
  }

  //
  // remove
  //

  def unregister(rule: NormalRule): Unit = {
    if (!(rules contains rule)) return
    rules = rules filter (_ != rule)
    val remainingAtoms = (rules flatMap (_.atoms)).toSet[Atom]  // (allAtoms() still contains the atoms of the rule)
    (rule.atoms diff remainingAtoms) foreach unregister
    (rule.body intersect remainingAtoms) foreach removeDeprecatedCons(rule)
  }

  def removeDeprecatedCons(rule: NormalRule)(a: Atom): Unit = {
    //efficiency - better use data structure
    if (!(justifications(rule.head) exists (_.body contains a))) {
      cons(a) -= rule.head
    }
  }

  def unregister(a: Atom): Unit = {
    status remove a
    cons remove a
    supp remove a
    _atomsCache remove a
    //suppRule remove a
  }

  //
  // set
  //

  override def set(model: collection.immutable.Set[Atom]): Boolean = {
    invalidateModel()
    model foreach (status(_) = in)
    (allAtoms diff model) foreach (status(_) = out)
    try {
      atomsNeedingSupp() foreach setSupport
    } catch {
      case e: IncrementalUpdateFailureException => {
        invalidateModel()
        return false
      }
    }
    true
  }

  def setSupport(a: Atom) {
    status(a) match {
      case `in` => setInSupport(a)
      case `out` => setOutSupport(a)
      case `unknown` => supp(a) = Set()
    }
  }

  def setInSupport(a: Atom) = justifications(a) find valid match {
    case Some(rule) => supp(a) = Set() ++ rule.body
    case _ => throw new IncrementalUpdateFailureException()
  }

  def setOutSupport(a: Atom) {
    val maybeAtoms: List[Option[Atom]] = justifications(a) map (findSpoiler(_))
    if (maybeAtoms exists (_.isEmpty)) {
      throw new IncrementalUpdateFailureException("could not find spoiler for every justification of atom "+a)
    }
    supp(a) = Set() ++ maybeAtoms map (_.get)
  }

}
