package jtms

import core._

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map, Set}

object TMN {

  def apply(P: Program) = {
    val tmn = new TMN()
    P.rules foreach tmn.add
    tmn
  }

}

/**
  * truth maintenance network
  * Created by hb on 12/22/15.
  */
case class TMN() {

  var rules: Set[Rule]= Set() //TODO (hb) use list later

  val Cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val Supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]] //TODO pos vs neg
  val SuppRule: Map[Atom, Option[Rule]] = new HashMap[Atom, Option[Rule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status] //at least 'in' consequence of SuppRule

  def registerAtom(a: Atom): Unit = {
    if (!status.isDefinedAt(a)) status(a) = out
    if (!Cons.isDefinedAt(a)) Cons(a) = Set[Atom]()
    if (!Supp.isDefinedAt(a)) Supp(a) = Set[Atom]()
    if (!SuppRule.isDefinedAt(a)) SuppRule(a) = None
  }

  def atoms() = Cons.keySet

  def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists (_.isInstanceOf[ContradictionAtom])) {
      return None
    }
    Some(atoms.toSet)
  }

  private def inAtoms() = (status.keys filter (status(_) == in)).toSet

  //TMS update algorithm
  def add(rule: Rule): Option[collection.immutable.Set[Atom]] = {

    register(rule)

    if (noStatusUpdate(rule)) {
      return Some(collection.immutable.Set())
    }

    updateBeliefs2(repercussions(rule.head) + rule.head)
  }

  def register(rule: Rule): Unit = {
    rules += rule
    rule.atoms foreach registerAtom
    rule.body foreach (Cons(_) += rule.head)
  }

  def noStatusUpdate(rule: Rule): Boolean = {
    if (status.head == in) return true
    //ignore invalid rule:
    findSpoiler(rule) match {
      case Some(spoiler) => { Supp(rule.head) += spoiler; true }
      case None => false
    }
  }

  def updateBeliefs(atoms: Set[Atom]): Option[collection.immutable.Set[Atom]] = {

    def stateOfAtoms() = atoms map (a => (a, status(a))) toList

    val oldState = stateOfAtoms

    atoms foreach setUnknown //Marking the nodes
    atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
    atoms foreach fixAndPropagateStatus // Relaxing circularities

    if (!tryEnsureConsistency) return None

    val newState = stateOfAtoms
    val result = (oldState diff newState) map (_._1) toSet

    Some(result)

  }

  def updateBeliefs2(atoms: Set[Atom]): Option[collection.immutable.Set[Atom]] = {
    stateDiff {
      atoms foreach setUnknown //Marking the nodes
      atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
      atoms foreach fixAndPropagateStatus // Relaxing circularities
      tryEnsureConsistency
    }
  }

  def stateOfAtoms() = atoms map (a => (a, status(a))) toList

  def stateDiff(evaluation: => () => Boolean): Option[collection.immutable.Set[Atom]] = {

    val oldState = stateOfAtoms

    evaluation()

    if (inAtoms exists contradictionAtom) return None

    val newState = stateOfAtoms
    val result = (oldState diff newState) map (_._1) toSet

    Some(result)

  }

  def isInvalid(rule: Rule): Boolean = { //TODO
    findSpoiler(rule) match {
      case Some(spoiler) => { Supp(rule.head) += spoiler; true }
      case None => false
    }
  }

  //return false if methods leaves without contradiction
  def tryEnsureConsistency(): Boolean = {
    for (c <- inAtoms() filter contradictionAtom) {
      if (!DDB(c)) return false
    }
    return true
  }

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom]

  def rulesWithHead(h: Atom) = rules filter (_.head == h)

  //ACons(a) = {x ∈ Cons(a) | a ∈ Supp(x)}
  def ACons(a: Atom): Set[Atom] = Cons(a) filter (Supp(_).contains(a))

  def repercussions(n: Atom) = trans(ACons, n)

  def setIn(rule: Rule) = {
    status(rule.head) = in
    Supp(rule.head) = Set() ++ rule.body
    SuppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    status(a) = out
    Supp(a) = rulesWithHead(a) map (findSpoiler(_).get)
    SuppRule(a) = None
  }

  def setUnknown(atom: Atom): Unit = {
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

  def unknownCons(a: Atom) = Cons(a) filter (status(_) == unknown)

  def determineAndPropagateStatus(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (validation(a) || invalidation(a)) {
        unknownCons(a) foreach determineAndPropagateStatus
    }
  }

  def validation(a: Atom): Boolean = {
    rulesWithHead(a) find foundedValid match {
      case Some(rule) => { setIn(rule); true }
      case None => false
    }
  }

  def invalidation(a: Atom): Boolean = {
    if (rulesWithHead(a) forall foundedInvalid) {
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
      for (c <- (ACons(a) + a)) {
        setUnknown(c)
        fixAndPropagateStatus(c)
      }
      //TODO (HB) shouldn't it be setUnknown for all, then fix.. for all?
    }
  }

  def fix(a: Atom): Boolean = {
    rulesWithHead(a) find unfoundedValid match {
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
    val unknownPosAtoms = rulesWithHead(a) map { r => (r.pos find (status(_)==unknown)).get }
    unknownPosAtoms foreach setOut
    setOut(a)
  }

  def foundedValid(rule: Rule): Boolean = {
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))
  }

  def foundedInvalid(rule: Rule): Boolean = {
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))
  }

  def unfoundedValid(rule: Rule): Boolean = {
    (rule.pos forall (status(_) == in)) && (!(rule.neg exists (status(_) == in))) && (rule.neg exists (status(_) == unknown))
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

  //book chapter
  def findBacktrackingRule(maxAssumptions: Set[Atom]): Option[RuleFromBacktracking] = {
    val n = SuppRule(maxAssumptions.head).get.neg.head

    val suppRules = maxAssumptions map (SuppRule(_).get)
    val pos = suppRules flatMap (_.pos)
    val neg = (suppRules flatMap (_.neg)) - n
    val rule = RuleFromBacktracking(pos,neg,n)

    assert(foundedValid(rule))

    Some(rule)
  }

  def findBacktrackingRule2(maxAssumptions: Set[Atom]): Option[RuleFromBacktracking] = {

    val suppRules = (maxAssumptions map (SuppRule(_).get)).toSet
    var rule:Option[RuleFromBacktracking] = None
    var candidates = List[Atom]() ++ maxAssumptions

    while (rule == None && !candidates.isEmpty) {
      val h = candidates.head
      candidates = candidates.tail
      rule = createValidRuleForBacktracking(suppRules, h)
    }

    rule
  }

  def createValidRuleForBacktracking(suppRules: collection.immutable.Set[Rule], h: Atom): Option[RuleFromBacktracking] = {

    val pos = suppRules flatMap (_.pos)
    val neg = (suppRules flatMap (_.neg)) - h

    if (pos.isEmpty && neg.isEmpty) return None
    val rule = RuleFromBacktracking(pos,neg,h)

    if (this.rules contains rule) return None //TODO (hb)

    assert(foundedValid(rule))

    Some(rule)
  }

  def antecedents(a: Atom): Set[Atom] = {
    if (status(a) == in) return Supp(a)
    Set()
  }

  def foundations(a: Atom) = trans(antecedents, a)

  def isAssumption(a: Atom) = (status(a) == in) && !SuppRule(a).get.neg.isEmpty

//  def suppRules(atoms: Set[Atom]) = {
//    SuppRule.filterKeys(atoms.contains(_)).values.map(_.get).toSet
//  }


  //TODO (hb) use List instead of Set s.t. consecutive removals amount to undo (same results as before)
  //in particular: Rule: List of pos/neg body atoms
  def remove(rule: Rule) = {

    val head = rule.head

    val rulesFromBacktracking = rules.filter(_.isInstanceOf[RuleFromBacktracking])

    def affectedAtoms(a: Atom) = repercussions(a) + a

    var L = affectedAtoms(head) ++ rulesFromBacktracking.flatMap(x => affectedAtoms(x.head))

    def removeRule(rule: Rule) = {
      for (m <- rule.body) {
        Cons(m) -= rule.head //TODO (HB) not necessarily
      }

      rules -= rule
    }

    removeRule(rule)

    rulesFromBacktracking.foreach(removeRule)

    // if no other rule exists containing the atom - remove it completely
    if (!rules.exists(_.atoms.contains(head))) {
      //atoms -= head
      status.remove(head)
      Supp.remove(head)
      SuppRule.remove(head)
      Cons.remove(head)
      L -= head
    }

    this.updateBeliefs(L)
  }

  // ----------------- stuff below might not be needed ----------------

  /** @return true if M is admissible **/
  //TODO (HB) review need of method
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
    val rules = rulesWithHead(n).filter(rule => rule.pos.subsetOf(MSub) && rule.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  def selectAtom(atoms: collection.immutable.Set[Atom]): Option[Atom] = {
    if (atoms.isEmpty)
      return None

    //TODO what about the ordering?
    Some(atoms.head)
  }

  def selectRule(rules: collection.mutable.Set[Rule]): Option[Rule] = {
    if (rules.isEmpty)
      return None

    //TODO what about the ordering?
    Some(rules.head)
  }

  def SuppTrans(n: Atom) = trans(Supp, n)

}