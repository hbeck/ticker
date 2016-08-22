package jtms

import core.Atom
import core.asp.NormalRule

import scala.util.Random

/**
  * Created by hb on 6/10/16.
  */
abstract class JtmsAbstraction(random: Random = new Random()) extends Jtms with ChoiceControl {

  override def allAtoms() = __allAtoms

  var __allAtoms: Set[Atom] = Set()

  override def justifications(a: Atom) = __justifications(a)

  var __justifications: Map[Atom, Set[NormalRule]] = Map.empty.withDefaultValue(Set())

  var __rulesAtomsOccursIn: Map[Atom, Set[NormalRule]] = Map.empty.withDefaultValue(Set()) //TODO we need to delete atoms too (grounding!)

  var __atomsWithStatus: Map[Status, Set[Atom]] = Map.empty.withDefaultValue(Set())

  override def inAtoms() = __atomsWithStatus(in)

  override def outAtoms(): Set[Atom] = __atomsWithStatus(out)

  override def unknownAtoms(): Set[Atom] = __atomsWithStatus(unknown)

  override def hasUnknown(): Boolean = unknownAtoms().nonEmpty

  def update(atoms: Set[Atom])

  //based on JTMS update algorithm
  override def add(rule: NormalRule): Unit = {
    register(rule)
    if (inconsistent()) {
      update(unknownAtoms() + rule.head)
    } else {
      if (status(rule.head) == in) {
        if (valid(rule)) {
          //difference to original; optimization for sliding time-based window (support always by latest)
          setIn(rule)
        }
        return
      }
      if (invalid(rule)) {
        //supp(rule.head) += findSpoiler(rule).get; return
        supp = supp.updated(rule.head, supp(rule.head) + findSpoiler(rule).get)
        return
      }
      val atoms = repercussions(rule.head) + rule.head
      update(atoms)
    }
  }

  override def remove(rule: NormalRule): Unit = {
    unregister(rule)
    if (inconsistent()) {
      val h = if (allAtoms contains rule.head) Set(rule.head) else Set()
      update(unknownAtoms() ++ h)
    } else {
      if (!(allAtoms contains rule.head)) return
      if (status(rule.head) == out) return
      //this should save some time!:
      if (suppRule(rule.head).isDefined && suppRule(rule.head).get != rule) return //.isDefined needed if previous state was inconsistent
      val atoms = repercussions(rule.head) + rule.head
      update(atoms)
    }
  }

  def recompute(): Unit = {
    update(unknownAtoms())
  }

  override def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None //not dealt with; left for old test-cases
    if (hasUnknown()) return None
    Some(atoms)
  }

  def inconsistent(): Boolean = unknownAtoms().nonEmpty

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
      if (recordStatusSeq) statusSeq = statusSeq :+ (a, out, "register")

      __allAtoms = __allAtoms + a

      status = status.updated(a, out)
      __atomsWithStatus = __atomsWithStatus.updated(out, __atomsWithStatus(out) + a)

      cons = cons.updated(a, Set[Atom]())
      supp = supp.updated(a, Set[Atom]())
      suppRule = suppRule.updated(a, None)
      //      status(a) = out
      //      cons(a) = Set[Atom]()
      //      supp(a) = Set[Atom]()
      //      suppRule(a) = None
    }
  }

  def invalidateModel(): Unit = {
    atomsNeedingSupp foreach setUnknown
  }

  def setIn(rule: NormalRule) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rule.head, in, "set")
    __updateStatus(rule.head, in)
    supp = supp.updated(rule.head, rule.body)
    suppRule = suppRule.updated(rule.head, Some(rule))
    //    status(rule.head) = in
    //    supp(rule.head) = Set() ++ rule.body
    //    suppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (a, out, "set")
    __updateStatus(a, out)
    setOutSupport(a)
    suppRule = suppRule.updated(a, None)
    //    status(a) = out
    //    setOutSupport(a)
    //    suppRule(a) = None
  }

  def setUnknown(a: Atom) = {
    __updateStatus(a, unknown)
    supp = supp.updated(a, Set())
    suppRule = suppRule.updated(a, None)
    //    status(a) = unknown
    //    supp(a) = Set()
    //    suppRule(a) = None
  }

  def __updateStatus(a: Atom, newStatus: Status): Unit = {

    val oldStatus = status(a)

    if (oldStatus != newStatus) {
      status = status.updated(a, newStatus)
      __atomsWithStatus = __atomsWithStatus.updated(newStatus, __atomsWithStatus(newStatus) + a)
      __atomsWithStatus = __atomsWithStatus.updated(oldStatus, __atomsWithStatus(oldStatus) - a)
    }

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
    __atomsWithStatus = __atomsWithStatus.updated(oldStatus, __atomsWithStatus(oldStatus) - a)

    status = status - a
    cons = cons - a
    supp = supp - a
    suppRule = suppRule - a
    //    status remove a
    //    cons remove a
    //    supp remove a
    //    suppRule remove a
  }

  var __cleanup = 0;

  def  __cleanupSupportingData(force: Boolean = false): Unit = {
    __cleanup = __cleanup + 1
    if (__cleanup % 1000 == 0 || force) {
      __cleanup = 0

      __justifications = __justifications filter (r => r._2.nonEmpty)
      __rulesAtomsOccursIn = __rulesAtomsOccursIn filter (a => a._2.nonEmpty)
    }
  }

  //
  // set
  //

  override def set(model: collection.immutable.Set[Atom]): Boolean = {
    invalidateModel()
    //model foreach (status(_) = in)
    model foreach { atom =>
      //status = status.updated(atom,in)
      __updateStatus(atom, in)
    }
    //(allAtoms diff model) foreach (status(_) = out)
    (allAtoms diff model) foreach { atom =>
      //status = status.updated(atom,out)
      __updateStatus(atom, out)
    }
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
      case `unknown` => supp = supp.updated(a, Set()) //supp(a) = Set()
    }
  }

  def setInSupport(a: Atom) = justifications(a) find valid match {
    case Some(rule) => supp = supp.updated(a, rule.body) //supp(a) = Set() ++ rule.body
    case _ => throw new IncrementalUpdateFailureException()
  }

  def setOutSupport(a: Atom) {
    val maybeAtoms: Set[Option[Atom]] = justifications(a) map findSpoiler
    if (maybeAtoms exists (_.isEmpty)) {
      throw new IncrementalUpdateFailureException("could not find spoiler for every justification of atom " + a)
    }
    //supp(a) = Set() ++ maybeAtoms map (_.get)
    supp = supp.updated(a, Set() ++ maybeAtoms map (_.get))
  }

}
