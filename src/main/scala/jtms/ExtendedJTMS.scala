package jtms

import core._

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map, Set}

object ExtendedJTMS {

  def apply(P: Program): ExtendedJTMS = {
    val net = new ExtendedJTMS()
    P.rules foreach net.add
    net
  }

}

/**
  * In addition to JTMS, ExtendedJTMS has a remove method.
  * Works in two modes, i) according to Doyle/Beierle and ii) stepwise, suitable if remove is used
  *
  */
case class ExtendedJTMS() {

  sealed trait UpdateStrategy
  object UpdateStrategyDoyle extends UpdateStrategy //only works for add()
  object UpdateStrategyStepwise extends UpdateStrategy

  var updateStrategy: UpdateStrategy = UpdateStrategyStepwise

  //

  var rules: List[Rule] = List()

  val cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val suppRule: Map[Atom, Option[Rule]] = new HashMap[Atom, Option[Rule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status] //at least 'in' consequence of SuppRule

  def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None //not dealt with
    Some(atoms.toSet)
  }

  def justifications(h: Atom) = rules filter (_.head == h)

  def atoms() = cons.keySet

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom] || a == Falsum

  def inAtoms() = status.keys filter (status(_) == in)

  def unknownAtoms() = status.keys filter (status(_) == unknown)

  //ACons(a) = {x ∈ Cons(a) | a ∈ Supp(x)}
  def aff(a: Atom): Set[Atom] = cons(a) filter (supp(_) contains a)

  def repercussions(a: Atom) = trans(aff, a)

  def antecedents(a: Atom): Set[Atom] = {
    if (status(a) == in) return supp(a)
    Set()
  }

  def foundations(a: Atom) = trans(antecedents, a)

  def ancestors(a: Atom) = trans(supp, a)

  def isAssumption(a: Atom) = (status(a) == in) && !suppRule(a).get.neg.isEmpty

  def unknownCons(a: Atom) = cons(a) filter (status(_) == unknown)

  def valid(rule: Rule) =
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))

  def invalid(rule: Rule) =
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))

  def unfounded(rule: Rule) =
    (rule.pos forall (status(_) == in)) && (!(rule.neg exists (status(_) == in)))

  //based on JTMS update algorithm
  def add(rule: Rule): Unit = {
    register(rule)
    if (status(rule.head) == in) return
    if (invalid(rule)) { supp(rule.head) += findSpoiler(rule).get; return }
    val ats = repercussions(rule.head) + rule.head
    updateBeliefs(ats)
  }

  def register(rule: Rule): Unit = {
    if (rules contains rule) return //list representation!
    rules = rules :+ rule
    rule.atoms foreach register
    rule.body foreach (cons(_) += rule.head)
  }

  def register(a: Atom): Unit = {
    if (!status.isDefinedAt(a)) status(a) = out
    if (!cons.isDefinedAt(a)) cons(a) = Set[Atom]()
    if (!supp.isDefinedAt(a)) supp(a) = Set[Atom]()
    if (!suppRule.isDefinedAt(a)) suppRule(a) = None
  }

  def updateBeliefs(atoms: Set[Atom]): Unit = {
    updateStrategy match {
      case `UpdateStrategyDoyle` => updateDoyle(atoms)
      case `UpdateStrategyStepwise` => updateStepwise(atoms)
    }
  }

  def updateDoyle(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown //Marking the nodes
    atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
    atoms foreach fixAndPropagateStatus // Relaxing circularities (might lead to contradictions)
    //tryEnsureConsistency
  }

  def updateStepwise(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown
    while (hasUnknown) {
      unknownAtomsList foreach determineAndPropagateStatus
      val atom = unknownAtomsList.headOption
      if (atom.isDefined) {
        fixAndDetermineAndPropagateStatus(atom.get)
      }
    }
  }

  def hasUnknown = atoms exists (status(_) == unknown)

  def unknownAtomsList = unknownAtoms.toList sortWith ((u1,u2) => contradictionAtom(u1))

  def setIn(rule: Rule) = {
    status(rule.head) = in
    supp(rule.head) = Set() ++ rule.body
    suppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    status(a) = out
    val maybeAtoms: List[Option[Atom]] = justifications(a) map (findSpoiler(_))
    supp(a) = Set() ++ (maybeAtoms filter (_.isDefined)) map (_.get)
    //supp(a) = Set() ++ (justifications(a) map (findSpoiler(_).get)) //problematic: a :- not b; b :- not a.
    suppRule(a) = None
  }

  def setUnknown(atom: Atom) = {
    status(atom) = unknown
    supp(atom) = Set()
    suppRule(atom) = None
  }

  def findSpoiler(rule: Rule): Option[Atom] = {
    if (math.random < 0.5) {
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

  def determineAndPropagateStatus(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (validation(a) || invalidation(a))
      unknownCons(a) foreach determineAndPropagateStatus
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

  def fixAndPropagateStatus(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (fix(a)) {
      unknownCons(a) foreach fixAndPropagateStatus
    } else {
      val affected = aff(a) + a
      affected foreach setUnknown
      affected foreach fixAndPropagateStatus
    }
  }

  def fixAndDetermineAndPropagateStatus(a: Atom): Unit = {
    if (fix(a)) {
      unknownCons(a) foreach determineAndPropagateStatus
    } else {
      val affected = aff(a) + a
      affected foreach setUnknown
    }
  }

  def fix(a: Atom): Boolean = {
    justifications(a) find unfounded match {
      case Some(rule) => {
          if (aff(a).isEmpty) fixIn(rule)
          else return false
      }
      case None => fixOut(a)
    }
    true
  }

  def fixIn(unfoundedValidRule: Rule) = {
    unfoundedValidRule.neg filter (status(_) == unknown) foreach setOut //fix ancestors
    setIn(unfoundedValidRule)
  }

  def fixOut(a: Atom) = {
    val unknownPosAtoms = updateStrategy match {
      case `UpdateStrategyDoyle` => {
        val maybeAtoms: List[Option[Atom]] = justifications(a) map { r => (r.pos find (status(_)==unknown)) }
        (maybeAtoms filter (_.isDefined)) map (_.get)
      }
      case `UpdateStrategyStepwise` => justifications(a) map { r => (r.pos find (status(_) == unknown)).get }
    }
    unknownPosAtoms foreach setOut //fix ancestors
    //note that only positive body atoms are used to create a spoilers, since a rule with an empty body
    //where the negative body is out/unknown is
    setOut(a)
  }

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

  def remove(rule: Rule): Unit = {
    unregister(rule)
    if (!(atoms contains rule.head)) return
    if (status(rule.head) == out) return
    if (suppRule(rule.head).get != rule) return
    val ats = repercussions(rule.head) + rule.head
    updateBeliefs(ats)
  }

  def unregister(rule: Rule): Unit = {
    if (!(rules contains rule)) return
    rules = rules filterNot (_ == rule)
    rule.atoms foreach conditionalUnregister
    conditionalConsRemove(rule)
  }

  //precondition: rule is already deleted from this.rules
  def conditionalConsRemove(rule: Rule): Unit = {
    for (a <- rule.body) {
      if (!(justifications(rule.head) exists (_.body contains a))) cons(a) -= rule.head //efficiency - better use data structure
    }
  }

  def conditionalUnregister(a: Atom): Unit = {
    val A = (rules flatMap (r => r.body + r.head)).toSet
    if (!(A contains a)) unregister(a)
  }

  def unregister(a: Atom): Unit = {
    status remove a
    cons remove a
    supp remove a
    suppRule remove a
  }

  // ----------------- test stuff or stuff that might not be needed ----------------

  /** @return true if M is admissible **/
  def set(M: collection.immutable.Set[Atom]): Boolean = {
  val m = M.toList
    for (i <- 0 to M.size - 1) {
      val rule: Option[Rule] = findSuppRule(m, i)
      if (rule.isEmpty) {
        return false
      }
      setIn(rule.get)
    }
    for (n <- atoms diff M) {
      setOut(n)
    }
    true
  }

  def isFounded(atoms:scala.collection.immutable.Set[Atom])={
    false
  }


  /** takes atoms at list M index idx and tries to find a valid rule
    * that is founded wrt indexes 0..idx-1
    */
  def findSuppRule(M: List[Atom], idx: Int): Option[Rule] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    val rules = justifications(n) filter (rule => rule.pos.subsetOf(MSub) && rule.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  def selectRule(rules: List[Rule]): Option[Rule] = {
    if (rules.isEmpty)
      return None
    Some(rules.head)
  }

}