package jtms

import core._

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map, Set}

object JTMNRefactored {

  def apply(P: Program): JTMNRefactored = {
    val tmn = new JTMNRefactored()
    P.rules foreach tmn.add
    tmn
  }

}

/**
  * justification-based truth maintenance network
  *
  * based book chapter from Beierle and Kern-Isberner
  *
  * Created by hb on 12/22/15.
  */
case class JTMNRefactored() {

  var rules: List[Rule] = List()

  val Cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val Supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val SuppRule: Map[Atom, Option[Rule]] = new HashMap[Atom, Option[Rule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status] //at least 'in' consequence of SuppRule

  def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None
    Some(atoms.toSet)
  }

  def justifications(h: Atom) = rules filter (_.head == h)

  def atoms() = Cons.keySet

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom]

  def inAtoms() = status.keys filter (status(_) == in)

  def unknownAtoms() = status.keys filter (status(_) == unknown)

  //ACons(a) = {x ∈ Cons(a) | a ∈ Supp(x)}
  def ACons(a: Atom): Set[Atom] = Cons(a) filter (Supp(_) contains a)

  def repercussions(a: Atom) = trans(ACons, a)

  def antecedents(a: Atom): Set[Atom] = {
    if (status(a) == in) return Supp(a)
    Set()
  }

  def foundations(a: Atom) = trans(antecedents, a)

  def ancestors(a: Atom) = trans(Supp, a)

  def isAssumption(a: Atom) = (status(a) == in) && !SuppRule(a).get.neg.isEmpty

  def unknownCons(a: Atom) = Cons(a) filter (status(_) == unknown)

  def foundedValid(rule: Rule) =
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))

  def foundedInvalid(rule: Rule) =
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))

  def unfoundedValid(rule: Rule) =
    (rule.pos forall (status(_) == in)) && (!(rule.neg exists (status(_) == in)))

  //JTMS update algorithm
  def add(rule: Rule): Unit = {
    register(rule)
    if (status(rule.head) == in || invalid(rule)) return
    val atoms = repercussions(rule.head) + rule.head
    updateBeliefs(atoms)
  }

  def register(rule: Rule): Unit = {
    if (rules contains rule) return //list representation!
    rules = rules :+ rule
    rule.atoms foreach register
    rule.body foreach (Cons(_) += rule.head)
  }

  def register(a: Atom): Unit = {
    if (!status.isDefinedAt(a)) status(a) = out
    if (!Cons.isDefinedAt(a)) Cons(a) = Set[Atom]()
    if (!Supp.isDefinedAt(a)) Supp(a) = Set[Atom]()
    if (!SuppRule.isDefinedAt(a)) SuppRule(a) = None
  }

  def invalid(rule: Rule) = findSpoiler(rule) match {
    case Some(spoiler) => Supp(rule.head) += spoiler; true
    case None => false
  }

  def updateBeliefs(atoms: Set[Atom]): Boolean = {
    atoms foreach setUnknown //Marking the nodes
    atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
    atoms foreach fixAndPropagateStatus // Relaxing circularities (might lead to contradictions)
    tryEnsureConsistency
  }

  def setIn(rule: Rule) = {
    status(rule.head) = in
    Supp(rule.head) = Set() ++ rule.body
    SuppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    status(a) = out
    val maybeAtoms: List[Option[Atom]] = justifications(a) map (findSpoiler(_))
    Supp(a) = Set() ++ (maybeAtoms filter (_.isDefined)) map (_.get)
    //Supp(a) = Set() ++ (justifications(a) map (findSpoiler(_).get))
    SuppRule(a) = None
  }

  def setUnknown(atom: Atom) = {
    status(atom) = unknown
    Supp(atom) = Set()
    SuppRule(atom) = None
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
    justifications(a) find foundedValid match {
      case Some(rule) => setIn(rule); true
      case None => false
    }
  }

  def invalidation(a: Atom): Boolean = {
    if (justifications(a) forall foundedInvalid) {
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
      val affected = ACons(a) + a
      affected foreach setUnknown
      affected foreach fixAndPropagateStatus
    }
  }

  def fix(a: Atom): Boolean = {
    justifications(a) find unfoundedValid match {
      case Some(rule) => {
          if (ACons(a).isEmpty) fixIn(rule)
          else return false
      }
      case None => fixOut(a)
    }
    true
  }

  def fixIn(unfoundedValidRule: Rule) = {
    unfoundedValidRule.neg filter (status(_) == unknown) foreach setOut //create foundation
    setIn(unfoundedValidRule)
  }

  def fixOut(a: Atom) = {
    //val unknownPosAtoms = justifications(a) map { r => (r.pos find (status(_)==unknown)).get }
    val maybeAtoms: List[Option[Atom]] = justifications(a) map { r => (r.pos find (status(_)==unknown)) }
    val unknownPosAtoms = (maybeAtoms filter (_.isDefined)) map (_.get)
    unknownPosAtoms foreach setOut //create foundation
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

  //return false if called DDB method leaves without resolving a contradiction
  def tryEnsureConsistency(): Boolean = {
    for (c <- inAtoms() filter contradictionAtom) {
      if (!DDB(c)) return false
    }
    true
  }

  //return true if method leaves with status(c) != in
  def DDB(c: Atom): Boolean = {

    if (status(c) != in) return true

    val asms = foundations(c) filter isAssumption
    val maxAssumptions = asms filter { a =>
      ! ((asms - a) exists (b => foundations(b) contains a))
    }

    if (maxAssumptions.isEmpty)
      return false //contradiction cannot be solved

    findBacktrackingRule(maxAssumptions) match {
      case Some(rule) => { add(rule); return true }
      case None => return false
    }

  }

  def findBacktrackingRule(maxAssumptions: Set[Atom]): Option[RuleFromBacktracking] = {

    val culprit = maxAssumptions.head
    val n = SuppRule(culprit).get.neg.head //(all .neg have status out at this point)

    val suppRules = maxAssumptions map (SuppRule(_).get)
    val pos = suppRules flatMap (_.pos)
    val neg = (suppRules flatMap (_.neg)) - n
    val rule = RuleFromBacktracking(pos, neg, n)

    Some(rule)
  }


  /* ----------------------- in progress ... ------------------------------------- */

  // ----------------- test stuff or stuff that might not be needed ----------------

  /** @return true if M is admissible **/
  def set(M: collection.immutable.Set[Atom]): Boolean = { //TODO (HB) Set vs List. Always list for order?
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

  /** takes atoms at list M index idx and tries to find a valid rule
    * that is founded wrt indexes 0..idx-1
    */
  def findSuppRule(M: List[Atom], idx: Int): Option[Rule] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    val rules = justifications(n).filter(rule => rule.pos.subsetOf(MSub) && rule.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  def selectRule(rules: List[Rule]): Option[Rule] = {
    if (rules.isEmpty)
      return None
    Some(rules.head)
  }

}