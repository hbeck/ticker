package jtms

import core._
import core.asp.{NormalProgram, NormalRule}

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map, Set}

object ExtendedJtms {

  def apply(P: NormalProgram): ExtendedJtms = {
    val net = new ExtendedJtms()
    P.rules foreach net.add
    net
  }

}

/**
  * In addition DO JTMS, ExtendedJTMS has a remove method.
  * Works in two modes, i) according to Doyle/Beierle and ii) stepwise, suitable if remove is used
  *
  */
case class ExtendedJtms() {

  sealed trait UpdateStrategy
  object UpdateStrategyDoyle extends UpdateStrategy //only works for add()
  object UpdateStrategyStepwise extends UpdateStrategy

  var updateStrategy: UpdateStrategy = UpdateStrategyStepwise

  var doSemanticsCheck = true //introduced while debugging remove problems

  //based on JTMS update algorithm
  def add(rule: NormalRule): Unit = {
    register(rule)
    if (status(rule.head) == in) return
    if (invalid(rule)) { supp(rule.head) += findSpoiler(rule).get; return }
    val ats = repercussions(rule.head) + rule.head
    updateBeliefs(ats)
  }

  def remove(rule: NormalRule): Unit = {
    unregister(rule)
    if (!(allAtoms contains rule.head)) return
    if (status(rule.head) == out) return
    if (suppRule(rule.head).get != rule) return
    val ats = repercussions(rule.head) + rule.head
    updateBeliefs(ats)
  }

  def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None //not dealt with
    Some(atoms.toSet)
  }

  //
  //
  //

  var rules: List[NormalRule] = List()

  val cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val suppRule: Map[Atom, Option[NormalRule]] = new HashMap[Atom, Option[NormalRule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status] //at least 'in' consequence of SuppRule

  //

  def justifications(h: Atom) = rules filter (_.head == h)

  def allAtoms() = cons.keySet

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom] || a == Falsum

  def inAtoms() = allAtoms filter (status(_) == in)

  def unknownAtoms() = allAtoms filter (status(_) == unknown)

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

  def valid(rule: NormalRule) =
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))

  def invalid(rule: NormalRule) =
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))

  def unfounded(rule: NormalRule) =
    (rule.pos forall (status(_) == in)) && (!(rule.neg exists (status(_) == in)))

  def register(rule: NormalRule): Unit = {
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
    checkSemantics()
  }

  def updateDoyle(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown //Marking the nodes
    atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
    atoms foreach fixAndPropagateStatus // Relaxing circularities (might lead to contradictions)
    //val atomList = (List[Atom]() ++ atoms).sortWith((x,y) => (x.toString=="d")) // TODO
    //atomList foreach fixAndPropagateStatus
  }

  def updateStepwise(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown
    while (hasUnknown) {
      unknownAtoms foreach determineAndPropagateStatus
      val atom = unknownAtoms.headOption
      if (atom.isDefined) {
        fixAndDetermineAndPropagateStatus(atom.get)
      }
    }
  }

  def hasUnknown = allAtoms exists (status(_) == unknown)

  def checkSemantics(): Unit = {
    if (!doSemanticsCheck) return
    val ruleHeads = rules map (_.head) toSet
    val facts = rules filter (_.isFact) map (_.head) toSet
    val atomsNeedingSupp = ruleHeads diff facts
    val badAtoms = atomsNeedingSupp filter (supp(_).isEmpty)
    if (!badAtoms.isEmpty) {
      println("the following atoms need but do not have a support")
      for (a <- badAtoms) {
        println(a)
      }
      throw new RuntimeException("no support for atoms "+badAtoms)
    }
  }

  def setIn(rule: NormalRule) = {
    status(rule.head) = in
    supp(rule.head) = Set() ++ rule.body
    suppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    status(a) = out
    supp(a) = Set() ++ (justifications(a) map (findSpoiler(_).get))
    suppRule(a) = None
  }

  def setUnknown(atom: Atom) = {
    status(atom) = unknown
    supp(atom) = Set()
    suppRule(atom) = None
  }

  def findSpoiler(rule: NormalRule): Option[Atom] = {
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
      aff(a) foreach setUnknown
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

  def fixIn(unfoundedRule: NormalRule): Unit = {
    setIn(unfoundedRule)
    unfoundedRule.neg filter (status(_) == unknown) foreach fixOut //fix ancestors TODO: write up has setOut, need fixOut
    /* not that setIn here has to be called first. consider
       a :- not b.
       b :- not a. ,
       where the choice is for status(a)=in. then, this status needs to be available
       when the spoiler for rule b :- not a is sought.
     */
  }

  def fixOut(a: Atom): Unit = {
    status(a) = out
    val maybeAtoms: List[Option[Atom]] = justifications(a) map { r => (r.pos find (status(_)==unknown)) }
    val unknownPosAtoms = (maybeAtoms filter (_.isDefined)) map (_.get)
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

  def unregister(rule: NormalRule): Unit = {
    if (!(rules contains rule)) return
    rules = rules filterNot (_ == rule)
    //unregister deprecated rule atoms
    val A = (rules flatMap (r => r.body + r.head)).toSet //atoms() based on cons keys which is not yet updated
    for (a <- (rule.atoms diff A)) {
      unregister(a)
    }
    //remove deprecated cons information
    for (a <- (rule.body intersect A)) { //body atoms still in use
      //efficiency - better use data structure
      if (!(justifications(rule.head) exists (_.body contains a))) {
        cons(a) -= rule.head
      }
    }
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
      val rule: Option[NormalRule] = findSuppRule(m, i)
      if (rule.isEmpty) {
        return false
      }
      setIn(rule.get)
    }
    for (n <- allAtoms diff M) {
      setOut(n)
    }
    true
  }

  def isFounded(atoms: scala.collection.immutable.Set[Atom])={
    false
  }


  /** takes atoms at list M index idx and tries to find a valid rule
    * that is founded wrt indexes 0..idx-1
    */
  def findSuppRule(M: List[Atom], idx: Int): Option[NormalRule] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    val rules = justifications(n) filter (rule => rule.pos.subsetOf(MSub) && rule.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  def selectRule(rules: List[NormalRule]): Option[NormalRule] = {
    if (rules.isEmpty)
      return None
    Some(rules.head)
  }

}