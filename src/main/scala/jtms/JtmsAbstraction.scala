package jtms

import core.Atom
import core.asp.NormalRule

import scala.util.Random

/**
  * Created by hb on 6/10/16.
  */
abstract class JtmsAbstraction(random: Random = new Random()) extends Jtms with ChoiceControl {

  override def allAtoms() = _atomsCache
  var _atomsCache: Set[Atom]= Set()

  var _rulesLookupCache : Set[NormalRule] = Set()

  override def justifications(a: Atom) = _justificationLookupCache(a)
  var _justificationLookupCache: Map[Atom, Seq[NormalRule]] = Map.empty.withDefaultValue(Seq())

  var _atomUsedByRuleCache : Map[Atom, Set[NormalRule]] = Map.empty.withDefaultValue(Set()) //TODO we need to delete atoms too (grounding!)

  def update(atoms: Set[Atom])

  //based on JTMS update algorithm
  override def add(rule: NormalRule): Unit = {
    register(rule)
    if (inconsistent) {
      update(unknownAtoms()+rule.head)
    } else {
      if (status(rule.head) == in) {
        if (valid(rule)) { //difference to original; optimization for sliding time-based window (support always by latest)
          setIn(rule)
        }
        return
      }
      if (invalid(rule)) {
        //supp(rule.head) += findSpoiler(rule).get; return
        supp = supp.updated(rule.head,supp(rule.head)+findSpoiler(rule).get)
        return
      }
      val atoms = (repercussions(rule.head) + rule.head).toSet
      update(atoms)
    }
  }

  override def remove(rule: NormalRule): Unit = {
    unregister(rule)
    if (inconsistent) {
      val h = if (allAtoms contains rule.head) Set(rule.head) else Set()
      update(unknownAtoms()++h)
    } else {
      if (!(allAtoms contains rule.head)) return
      if (status(rule.head) == out) return
      //this should save some time!:
      if (suppRule(rule.head).isDefined && suppRule(rule.head).get != rule) return //.isDefined needed if previous state was inconsistent
      val atoms = (repercussions(rule.head) + rule.head).toSet
      update(atoms)
    }
  }

  def recompute(): Unit = {
    update(unknownAtoms())
  }

  override def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None //not dealt with; left for old test-cases
    if (hasUnknown) return None
    Some(atoms)
  }

  def inconsistent(): Boolean = {
    !unknownAtoms.isEmpty
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

  //return true iff rule is new
  def register(rule: NormalRule): Boolean = {
    if (_rulesLookupCache contains rule) return false //list representation!

    rules = rules :+ rule

    _rulesLookupCache = _rulesLookupCache + rule
    _justificationLookupCache = _justificationLookupCache updated (rule.head, _justificationLookupCache(rule.head) :+ rule)

    //TODO find more specific name:
    val updatedMappings = rule.atoms map (a => (a, _atomUsedByRuleCache(a) + rule))
    _atomUsedByRuleCache = _atomUsedByRuleCache ++ updatedMappings

    rule.atoms foreach register
    //rule.body foreach (cons(_) += rule.head)
    rule.body foreach { atom =>
      cons = cons.updated(atom, cons(atom)+rule.head)
    }
    true
  }

  def register(a: Atom) {
    if (!status.isDefinedAt(a)) { //use this immediately as test whether the atom exists; all atoms need to have a status
      if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"register")
      _atomsCache = _atomsCache + a
      status = status.updated(a,out)
      cons = cons.updated(a,Set[Atom]())
      supp = supp.updated(a,Set[Atom]())
      suppRule = suppRule.updated(a,None)
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
    if (recordStatusSeq) statusSeq = statusSeq :+ (rule.head,in,"set")
    status = status.updated(rule.head,in)
    supp = supp.updated(rule.head,rule.body)
    suppRule = suppRule.updated(rule.head,Some(rule))
//    status(rule.head) = in
//    supp(rule.head) = Set() ++ rule.body
//    suppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"set")
    status = status.updated(a,out)
    setOutSupport(a)
    suppRule = suppRule.updated(a,None)
//    status(a) = out
//    setOutSupport(a)
//    suppRule(a) = None
  }

  def setUnknown(a: Atom) = {
    status = status.updated(a,unknown)
    supp = supp.updated(a,Set())
    suppRule = suppRule.updated(a,None)
//    status(a) = unknown
//    supp(a) = Set()
//    suppRule(a) = None
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
    if (!(_rulesLookupCache contains rule)) return false

    rules = rules filter (_ != rule)
    _rulesLookupCache = _rulesLookupCache - rule

    _justificationLookupCache = _justificationLookupCache updated (rule.head, _justificationLookupCache(rule.head) filter (_!= rule))

    //TODO more specific name
    val updatedMappings = rule.atoms map (a => (a, _atomUsedByRuleCache(a) - rule))
    _atomUsedByRuleCache = _atomUsedByRuleCache ++ updatedMappings

    val atomToBeRemoved = rule.atoms filter (a => _atomUsedByRuleCache(a).isEmpty)
    val remainingAtoms = _atomsCache diff atomToBeRemoved

    atomToBeRemoved foreach unregister
    (rule.body intersect remainingAtoms) foreach removeDeprecatedCons(rule)
    true
  }

  def removeDeprecatedCons(rule: NormalRule)(a: Atom): Unit = {
    //efficiency - better use data structure
    if (!(justifications(rule.head) exists (_.body contains a))) {
      //cons(a) -= rule.head
      cons = cons.updated(a,cons(a)-rule.head)
    }
  }

  def unregister(a: Atom): Unit = {
    _atomsCache = _atomsCache - a
    status = status - a
    cons = cons - a
    supp = supp - a
    suppRule = suppRule - a
//    status remove a
//    cons remove a
//    supp remove a
//    suppRule remove a
  }

  //
  // set
  //

  override def set(model: collection.immutable.Set[Atom]): Boolean = {
    invalidateModel()
    //model foreach (status(_) = in)
    model foreach { atom =>
      status = status.updated(atom,in)
    }
    //(allAtoms diff model) foreach (status(_) = out)
    (allAtoms diff model) foreach { atom =>
      status = status.updated(atom,out)
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
      case `unknown` => supp = supp.updated(a,Set()) //supp(a) = Set()
    }
  }

  def setInSupport(a: Atom) = justifications(a) find valid match {
    case Some(rule) => supp = supp.updated(a,rule.body) //supp(a) = Set() ++ rule.body
    case _ => throw new IncrementalUpdateFailureException()
  }

  def setOutSupport(a: Atom) {
    val maybeAtoms: Seq[Option[Atom]] = justifications(a) map (findSpoiler(_))
    if (maybeAtoms exists (_.isEmpty)) {
      throw new IncrementalUpdateFailureException("could not find spoiler for every justification of atom "+a)
    }
    //supp(a) = Set() ++ maybeAtoms map (_.get)
    supp = supp.updated(a, Set() ++ maybeAtoms map (_.get))
  }

}
