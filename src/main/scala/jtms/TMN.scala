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

  val fixInOwn = true
  val fixOutOwn = true
  val singleFix = false
  //if !singleFix, decide:
  val sortBeforeFix = false //TODO consistency with flag true for P3, P4, not with false
  //fixContrFirst = fixInOwn && fixOutOwn && !singleFix && sortBeforeFix

  //var loop: Boolean = false
  var consistent: Boolean = true

  def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    if (!consistent) return None
    //if (someNoneFactHasNoSupport) return None
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None
    Some(atoms.toSet)
  }

//  def someNoneFactHasNoSupport(): Boolean = {
//    val heads = (rules map (_.head)).toSet
//    val factHeads = (rules filter (r => r.pos.isEmpty && r.neg.isEmpty)) map (_.head)
//    val intensional = heads -- factHeads
//    //intensional exists (Supp(_).isEmpty)
//    val unsupported: Predef.Set[Atom] = intensional filter (Supp(_).isEmpty)
//    for (x <- unsupported) {
//      if (status(x) == in) {
//        justifications(x) find foundedValid match {
//          case Some(r) => setIn(r)
//          case None => return true
//        }
//      } else if (status(x) == out) {
//        justifications(x) find foundedInvalid match {
//          case Some(r) => setOut(x)
//          case None => return true
//        }
//      } else {
//        throw new RuntimeException("outsch")
//      }
//    }
//    return false
//  }

  def inAtoms() = status.keys filter (status(_) == in)

  def unknownAtoms() = status.keys filter (status(_) == unknown)

  //TMS update algorithm
  def add(rule: Rule): Unit = {
    if (!consistent) return

    register(rule)
    if (status(rule.head) == in || invalid(rule)) return
    val atoms = repercussions(rule.head) + rule.head
    if (singleFix) {
      updateBeliefs2(atoms)
    } else {
      updateBeliefs(atoms)
    }

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
    val ats = sortBeforeFix match {
        case true => (List[Atom]() ++ atoms) sortWith ((a1, a2) => contradictionAtom(a1))
        case false => atoms
      }
    ats foreach fixAndPropagateStatus // Relaxing circularities (might lead to contradictions)
    tryEnsureConsistency
  }

  def updateBeliefs2(atoms: Set[Atom]): Boolean = {
    atoms foreach setUnknown //Marking the nodes
    while (!unknownAtoms.isEmpty) {
      unknownAtoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
      if (!unknownAtoms.isEmpty) {
        fixAndDetermineAndPropagateStatus(unknownAtoms.head) // Relaxing circularities (might lead to contradictions)
      }
    }
    tryEnsureConsistency
  }

  def justifications(h: Atom) = rules filter (_.head == h)

  //ACons(a) = {x ∈ Cons(a) | a ∈ Supp(x)}
  def ACons(a: Atom): Set[Atom] = Cons(a) filter (Supp(_) contains a)

  def DCons(a: Atom): Set[Atom] = Cons(a) filter (d => (status(d) != unknown) && (justifications(d) exists unknownBodyAtomOtherThan(a)))

  def ContradictedDCons(a: Atom): Set[Atom] = DCons(a) filter (d => (status(d) == out) && (justifications(d) exists foundedValid))

  def unknownBodyAtomOtherThan(a: Atom)(r: Rule) = !((r.pos ++ r.neg - a) exists (status(_) == unknown))

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
      case Some(rule) => {
        setIn(rule)
        ContradictedDCons(a) foreach setUnknown
        true
      }
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
    if (status(a) != unknown) // || loop)
      return

    if (fix(a)) {
      unknownCons(a) foreach fixAndPropagateStatus
    } else {
      val affected = ACons(a) + a
      affected foreach setUnknown
      affected foreach fixAndPropagateStatus
    }
  }

  def fixAndDetermineAndPropagateStatus(a: Atom): Unit = {
    if (fix(a)) {
      unknownCons(a) foreach determineAndPropagateStatus
    } else {
      val affected = ACons(a) + a
      affected foreach setUnknown
    }
  }

  def fix(a: Atom): Boolean = {
    //if (loop) return false

    justifications(a) find unfoundedValid match {
      case Some(rule) => {
//        if (contradictionAtom(a)) {
//          //TODO
//        } else {
          //if (looping(rule)) return false
          if (ACons(a).isEmpty) {
            fixIn(rule)
            //ContradictedDCons(a) foreach setUnknown
          }
          else return false
//        }
      }
      case None => {
        fixOut(a)
        ContradictedDCons(a) foreach setUnknown
      }
    }
    true
  }

  def looping(rule: Rule): Boolean = {
    val anc = rule.pos flatMap ancestors //cannot simply say ancestors(a) because supp of r.pos supp will in general not be set
    if (anc contains rule.head) {
      //loop = true
      return true
    }
    return false
  }

  def fixIn(unfoundedValidRule: Rule) = {
    if (fixInOwn) {
      unfoundedValidRule.neg filter (status(_) == unknown) foreach setOut //create foundation
      setIn(unfoundedValidRule)
    } else {
      beierleFixIn(unfoundedValidRule: Rule)
    }
  }

  def beierleFixIn(unfoundedValidRule: Rule): Unit = {
    setIn(unfoundedValidRule)
    for (n <- unfoundedValidRule.neg) {
      if (status(n) == unknown) {
        status(n) = out
      }
    }
  }

  def fixOut(a: Atom) = {
    if (fixOutOwn) {
      //val unknownPosAtoms = justifications(a) map { r => (r.pos find (status(_)==unknown)).get }
      val maybeAtoms: List[Option[Atom]] = justifications(a) map { r => (r.pos find (status(_)==unknown)) }
      val unknownPosAtoms = (maybeAtoms filter (_.isDefined)) map (_.get)
      unknownPosAtoms foreach setOut //create foundation
      setOut(a)
    } else {
      beierleFixOut(a: Atom)
    }
  }

  def beierleFixOut(a: Atom): Unit = {
    status(a) = out
    for (rule <- justifications(a)) {
      val p = (rule.pos find (status(_)==unknown)).get
      status(p)=out
      /* "bestimme supp(n) entsprechend:" */
    }
    Supp(a) = Set() ++ (justifications(a) map (findSpoiler(_).get))
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

  //return false if called DDB method leaves without resolving a contradiction
  def tryEnsureConsistency(): Boolean = {
    for (c <- inAtoms() filter contradictionAtom) {
      if (!DDB(c)) return false
    }
    true
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

    findBacktrackingRule2(c,maxAssumptions) match {
      case Some(rule) => { add(rule); return true }
      case None => return false
    }

  }

  //modified book chapter variant
  def findBacktrackingRule2(c: Atom, maxAssumptions: Set[Atom]): Option[RuleFromBacktracking] = {

    var rule: Option[RuleFromBacktracking] = None
    var assumptions = Set[Atom]() ++ maxAssumptions
    val suppRules = maxAssumptions map (SuppRule(_).get)
    val pos = (suppRules flatMap (_.pos)) + c
    val negBase = suppRules flatMap (_.neg)
    while (rule == None && !assumptions.isEmpty) {
      val culprit = assumptions.head
      assumptions = assumptions-culprit
      var negAtoms = SuppRule(culprit).get.neg
      while (rule == None && !negAtoms.isEmpty) {
        val n = negAtoms.head
        negAtoms = negAtoms - n
        val r = RuleFromBacktracking(pos,negBase-n,n)
        if (!(rules contains r)) {
          rule = Some(r)
        }
      }
    }
    rule

    // case w/ot iterations:

//    val culprit = maxAssumptions.head
//    val n = SuppRule(culprit).get.neg.head //(all .neg have status out at this point)
//
//    val suppRules = maxAssumptions map (SuppRule(_).get)
//    val pos = (suppRules flatMap (_.pos)) + c
//    val neg = (suppRules flatMap (_.neg)) - n
//    val rule = RuleFromBacktracking(pos, neg, n)

    //Some(rule)
  }

  def findBacktrackingRule3(c: Atom, maxAssumptions: Set[Atom]): Option[RuleFromBacktracking] = {

    val ng = installNoGoodRule(c,maxAssumptions)

    val E = Predef.Set[Atom]()

    var rule: Option[RuleFromBacktracking] = None
    var assumptions = Set[Atom]() ++ maxAssumptions

    //val pos = E ++ maxAssumptions + c
    val pos = Predef.Set[Atom](ng) ++ maxAssumptions
    //val pos = E ++ maxAssumptions + c

    var countOuter = 0
    var countInner = 0

    while (rule == None && !assumptions.isEmpty && countOuter >= 0) {
      countOuter += 1; if (countOuter >= 2) println("outer: "+countOuter)
      val culprit = assumptions.head
      assumptions = assumptions-culprit
      val sr = SuppRule(culprit).get
      var negCands = Set[Atom]() ++ sr.neg
      while (rule == None && !negCands.isEmpty && countInner >= 0) {
        countInner += 1; if (countInner >= 2) println("inner: "+countInner)
        val n = negCands.head
        negCands = negCands - n
        val neg = (sr.neg filter (status(_) == out)) - n
        val r = RuleFromBacktracking(pos-culprit,neg,n)
        if (!(rules contains r)) {
          rule = Some(r)
        }
      }
    }
    rule
  }

  val noGoodRules: Map[(Atom,Set[Atom]), Rule] = new HashMap[(Atom,Set[Atom]), Rule]

  //return no-good itself
  def installNoGoodRule(c: Atom, maxAssumptions: Set[Atom]): Atom = {
    val rule = noGoodRules.get((c, maxAssumptions)) match {
      case Some(r) => r
      case None => {
        val pos = Predef.Set[Atom](c) ++ maxAssumptions
        val neg = Predef.Set[Atom]()
        val r = new RuleFromBacktracking(pos, neg, new ContradictionAtom("[" + c + ";" + maxAssumptions + "]"))
        noGoodRules((c, maxAssumptions)) = r
        register(r)
        r
      }
    }
    setIn(rule)
    rule.head
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

    this.updateBeliefs(L)
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
    val rules = justifications(n).filter(rule => rule.pos.subsetOf(MSub) && rule.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  def selectRule(rules: List[Rule]): Option[Rule] = {
    if (rules.isEmpty)
      return None
    Some(rules.head)
  }

}