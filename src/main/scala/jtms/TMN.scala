package jtms

import core._

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map, Set}

object TMN {

  def apply(P: Program): TMN = {
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

  var rules: List[Rule] = List()

  val Cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val Supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val SuppRule: Map[Atom, Option[Rule]] = new HashMap[Atom, Option[Rule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status] //at least 'in' consequence of SuppRule

  def atoms() = Cons.keySet

  def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    if (loop) return None
    if (someNoneFactHasNoSupport) return None
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None
    Some(atoms.toSet)
  }

  def someNoneFactHasNoSupport(): Boolean = {
    val heads = (rules map (_.head)).toSet
    val factHeads = (rules filter (r => r.pos.isEmpty && r.neg.isEmpty)) map (_.head)
    val intensional = heads -- factHeads
    //intensional exists (Supp(_).isEmpty)
    val unsupported: Predef.Set[Atom] = intensional filter (Supp(_).isEmpty)
    for (x <- unsupported) {
      if (status(x) == in) {
        justifications(x) find foundedValid match {
          case Some(r) => setIn(r)
          case None => return true
        }
      } else if (status(x) == out) {
        justifications(x) find foundedInvalid match {
          case Some(r) => setOut(x,false)
          case None => return true
        }
      } else {
        throw new RuntimeException("outsch")
      }
    }
    return false
  }

  def inAtoms() = status.keys filter (status(_) == in)

  //TMS update algorithm
  def add(rule: Rule): Unit = add(rule,true)

  var loop: Boolean = false

  def add(rule: Rule, withDDB: Boolean): Unit = {
    loop = false
    register(rule)
    if (status(rule.head) == in || invalid(rule)) return
    updateBeliefs(repercussions(rule.head) + rule.head,withDDB)
  }

  def register(rule: Rule): Unit = {
    if (rules contains rule) return //list representation!
    rules = rules :+ rule
    rule.atoms foreach registerAtom
    rule.body foreach (Cons(_) += rule.head)
  }

  def registerAtom(a: Atom): Unit = {
    if (!status.isDefinedAt(a)) status(a) = out
    if (!Cons.isDefinedAt(a)) Cons(a) = Set[Atom]()
    if (!Supp.isDefinedAt(a)) Supp(a) = Set[Atom]()
    if (!SuppRule.isDefinedAt(a)) SuppRule(a) = None
  }

  def invalid(rule: Rule) = findSpoiler(rule,false) match {
    case Some(spoiler) => Supp(rule.head) += spoiler; true
    case None => false
  }

  def updateBeliefs(atoms: Set[Atom], withDDB: Boolean) = {
    atoms foreach setUnknown //Marking the nodes
    atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
    atoms foreach fixAndPropagateStatus
    if (withDDB) tryEnsureConsistency
  }

  def justifications(h: Atom) = rules filter (_.head == h)

  //ACons(a) = {x ∈ Cons(a) | a ∈ Supp(x)}
  def ACons(a: Atom): Set[Atom] = Cons(a) filter (Supp(_).contains(a))

  def repercussions(a: Atom) = trans(ACons, a)

  def antecedents(a: Atom): Set[Atom] = {
    if (status(a) == in) return Supp(a)
    Set()
  }

  def foundations(a: Atom) = trans(antecedents, a)

  def ancestors(a: Atom) = trans(Supp, a)

  def isAssumption(a: Atom) = (status(a) == in) && !SuppRule(a).get.neg.isEmpty

  def unknownCons(a: Atom) = Cons(a) filter (status(_) == unknown)

  def setIn(rule: Rule) = {
    status(rule.head) = in
    Supp(rule.head) = Set() ++ rule.body
    SuppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom, allowUnknown: Boolean) = {
    status(a) = out
    val maybeAtoms: List[Option[Atom]] = justifications(a) map (findSpoiler(_,allowUnknown))
    Supp(a) = Set() ++ (maybeAtoms filter (_.isDefined)) map (_.get)
    //Supp(a) = Set() ++ (justifications(a) map (findSpoiler(_,allowUnknown).get))
    SuppRule(a) = None
  }

  def setUnknown(atom: Atom) = {
    status(atom) = unknown
    Supp(atom) = Set()
    SuppRule(atom) = None
  }

  def stat(a: Atom, s: Status, allowUnknown: Boolean): Boolean = {
    if (status(a)==s) return true
    if (allowUnknown && status(a)==unknown) return true
    false
  }

  def findSpoiler(rule: Rule, allowUnknown: Boolean): Option[Atom] = {
    if (math.random < 0.5) {
      rule.pos find (stat(_,out,allowUnknown)) match {
        case None => rule.neg find (stat(_,in,allowUnknown))
        case opt => opt
      }
    } else {
      rule.neg find (stat(_,in,allowUnknown)) match {
        case None => rule.pos find (stat(_,out,allowUnknown))
        case opt => opt
      }
    }
  }

//  def findSpoiler(rule: Rule): Option[Atom] = {
//    if (math.random < 0.5) {
//      rule.pos find (status(_) == out) match {
//        case None => rule.neg find (status(_) == in)
//        case opt => opt
//      }
//    } else {
//      rule.neg find (status(_) == in) match {
//        case None => rule.pos find (status(_) == out)
//        case opt => opt
//      }
//    }
//  }

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
      setOut(a,false)
      return true
    }
    false
  }

  def fixAndPropagateStatus(a: Atom): Unit = {
    if (status(a) != unknown || loop)
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
    if (loop) return false

    justifications(a) find unfoundedValid match {
      case Some(rule) => {
        val anc = rule.pos flatMap ancestors //cannot simply say ancestors(a) because supp of r.pos supp will not i.g. be set
        if (anc contains a) {
          loop = true
          return false
        }

        if (ACons(a).isEmpty) fixIn(rule)
        else return false
      }
      case None => fixOut(a)
    }
    true
  }

  def fixIn(unfoundedValidRule: Rule) = {
    unfoundedValidRule.neg filter (status(_) == unknown) foreach (setOut(_,true)) //create foundation //TODO diff
    //unfoundedValidRule.neg foreach (status(_) = out) //support not set
    setIn(unfoundedValidRule)
  }

  def fixOut(a: Atom) = {
    val maybeAtoms: List[Option[Atom]] = justifications(a) map { r => (r.pos find (status(_)==unknown)) }
    val unknownPosAtoms = (maybeAtoms filter (_.isDefined)) map (_.get)
    unknownPosAtoms foreach (setOut(_,false)) //create foundation
    //status(a) = out //support not set
//    val unknownPosAtoms = justifications(a) map { r => (r.pos find (status(_)==unknown)).get }
//    unknownPosAtoms foreach setOut //create foundation
    setOut(a,false)
  }

  def foundedValid(rule: Rule) =
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))

  def foundedInvalid(rule: Rule) =
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))

  def unfoundedValid(rule: Rule) =
    (rule.pos forall (status(_) == in)) && (!(rule.neg exists (status(_) == in)))
  //&& (rule.neg exists (status(_) == unknown))

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

  //return immediately if called DDB method leaves without resolving a contradiction
  def tryEnsureConsistency(): Unit = {
    for (c <- inAtoms() filter contradictionAtom) {
      if (!DDB(c)) return
    }
  }

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom]

  //return true if method leaves with status(c) != in
  def DDB(c: Atom): Boolean = {

    if (status(c) != in) return true

    val asms = foundations(c) filter isAssumption
    val maxAssumptions = asms filter { a =>
      ! ((asms - a) exists (b => foundations(b) contains a))
    }

    if (maxAssumptions.isEmpty)
      return false //contradiction cannot be solved

    findBacktrackingRule3(c,maxAssumptions) match {
      case Some(rule) => { add(rule); return true }
      case None => return false
    }

  }

  //book chapter variant
  def findBacktrackingRule(maxAssumptions: Set[Atom]): Option[RuleFromBacktracking] = {

    val culprit = maxAssumptions.head
    val n = SuppRule(culprit).get.neg.head //(all .neg have status out at this point)

    val suppRules = maxAssumptions map (SuppRule(_).get)
    val pos = suppRules flatMap (_.pos)
    val neg = (suppRules flatMap (_.neg)) - n
    val rule = RuleFromBacktracking(pos, neg, n)

    assert(foundedValid(rule))

    Some(rule)
  }

  def findBacktrackingRule3(c: Atom, maxAssumptions: Set[Atom]): Option[RuleFromBacktracking] = {

    //val noGood = new ContradictionAtom(""+System.currentTimeMillis()) //TODO (hb) store and potentially retrieve

    val E = Predef.Set[Atom]()

    //val ngr = new RuleFromBacktracking(E ++ maxAssumptions + c,E,noGood)
    //add(ngr,false)

    var rule: Option[RuleFromBacktracking] = None
    var assumptions = Set[Atom]() ++ maxAssumptions

    val pos = E ++ maxAssumptions + c

    while (rule == None && !assumptions.isEmpty) {
      val culprit = maxAssumptions.head
      assumptions = assumptions-culprit
      val sr = SuppRule(culprit).get
      var negCands = Set[Atom]() ++ sr.neg
      while (rule == None && !negCands.isEmpty) {
        val n = negCands.head
        negCands = negCands - n
        val neg = sr.neg - n
        val r = RuleFromBacktracking(pos,neg,n)
        if (!(rules contains r)) {
          rule = Some(r)
        }
      }
    }

    rule

//    val culprit = maxAssumptions.head
//    val sr = SuppRule(culprit).get
//    val n = sr.neg.head //(all .neg have status out at this point)

    //val suppRules = maxAssumptions map (SuppRule(_).get)
    //val pos = (suppRules flatMap (_.pos)) + c
    //val pos = E ++ maxAssumptions + noGood
    //val pos = E ++ maxAssumptions + c
    //val neg = (suppRules flatMap (_.neg)) - n
    //val neg = SuppRule(culprit).get.neg - n
    //val neg = sr.neg - n
    //val rule = RuleFromBacktracking(pos, neg, n) //TODO

//    assert(foundedValid(rule))
//
//    Some(rule)
  }

  /* ----------------------- in progress ... ------------------------------------- */

//  //towards a working variant ...
//  def findBacktrackingRule2(maxAssumptions: Set[Atom]): Option[RuleFromBacktracking] = {
//
//    var rule: Option[RuleFromBacktracking] = None
//    var assumptions = List[Atom]() ++ maxAssumptions
//
//    //try all variants (instead of greedy pick of book chapter)
//    //TODO (hb) refactor
//    while (rule == None && !assumptions.isEmpty) {
//      val h = assumptions.head
//      var nStarCandidates = Set[Atom]() ++ SuppRule(h).get.neg
//      val suppRules = (assumptions map (SuppRule(_).get)).toSet
//      while (rule == None && !nStarCandidates.isEmpty) {
//        val nStar = nStarCandidates.head
//        rule = createValidRuleForBacktracking(suppRules, nStar)
//        nStarCandidates = nStarCandidates.tail
//      }
//      assumptions = assumptions.tail
//    }
//
//    rule
//  }
//
//  def createValidRuleForBacktracking(suppRules: collection.immutable.Set[Rule], nStar: Atom): Option[RuleFromBacktracking] = {
//
//    val pos = suppRules flatMap (_.pos)
//    val neg = (suppRules flatMap (_.neg)) - nStar
//
//    val rule = RuleFromBacktracking(pos, neg, nStar)
//
//    if (permittingFoundation(rule)) Some(rule)
//    else None
//
//  }
//
//  def permittingFoundation(rule: Rule): Boolean = {
//    if (!rule.pos.isEmpty || !rule.neg.isEmpty) return true //crucial are facts
//    //easy case when no rule for this fact exists:
//    if (justifications(rule.head).isEmpty) return false
//    //TODO hard cases; the rule above does not work in general (e.g. the case a:- not b. b:- not a. :- a.)
//    //if (revConsTrans(rule.head).contains(rule.head)) return false
//    return false
//  }

  def revCons(a: Atom): Set[Atom] = Set() ++ Cons.keys filter (Cons(_) contains a)

  def revConsTrans(a: Atom) = trans(revCons, a)




  /* ---------------------------------- future stuff ------------------------------------- */

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

      rules = rules filter (_ != rule)
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

    this.updateBeliefs(L,true)
  }

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
      setOut(n,false)
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
    val rules = justifications(n).filter(rule => rule.pos.subsetOf(MSub) && rule.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  def selectRule(rules: List[Rule]): Option[Rule] = {
    if (rules.isEmpty)
      return None
    Some(rules.head)
  }

}