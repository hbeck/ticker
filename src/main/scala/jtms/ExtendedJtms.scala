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
  * In addition to JTMS, ExtendedJTMS has a remove method.
  * Works in two update modes,
  *
  * i) according to Doyle/Beierle and
  * ii) stepwise, where a deterministic step succeeds very choice (fix).
  *
  */
case class ExtendedJtms() {

  sealed trait UpdateStrategy
  object UpdateStrategyDoyle extends UpdateStrategy //only works for add()
  object UpdateStrategyStepwise extends UpdateStrategy

  var updateStrategy: UpdateStrategy = UpdateStrategyStepwise

  //for inspection:
  var doTmsSemanticsCheck = true //introduced while debugging remove problems
  var doSelfSupportCheck = true
  var doConsistencyCheck = true //detect wrong computation of odd loop, report inconsistency
  var shuffle = true
  var recordChoiceSeq = true
  var recordStatusSeq = true

  var choiceSeq = Seq[Atom]()
  var statusSeq = Seq[(Atom,Status,String)]()

  //based on JTMS update algorithm
  def add(rule: NormalRule): Unit = {
    register(rule)
    if (status(rule.head) == in) return
    if (invalid(rule)) { supp(rule.head) += findSpoiler(rule).get; return }
    val atoms = repercussions(rule.head) + rule.head
    updateBeliefs(atoms)
  }

  def remove(rule: NormalRule): Unit = {
    unregister(rule)
    if (!(allAtoms contains rule.head)) return
    if (status(rule.head) == out) return
    if (suppRule(rule.head).isDefined && suppRule(rule.head).get != rule) return //.isDefined needed if previous state was inconsistent
    val atoms = repercussions(rule.head) + rule.head
    updateBeliefs(atoms)
  }

  def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None //not dealt with
    if (hasUnknown) return None
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

  def allAtoms() = (rules flatMap (_.atoms)) toSet

  def facts() = rules filter (_.isFact) map (_.head) toSet

  def ruleHeads() = rules map (_.head) toSet

  def atomsNeedingSupp() = ruleHeads diff facts

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom] || a == Falsum

  def inAtoms() = allAtoms filter (status(_) == in)

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

  def isAssumption(a: Atom) = (status(a) == in) && !suppRule(a).get.neg.isEmpty

  def unknownCons(a: Atom) = cons(a) filter (status(_) == unknown)

  def valid(rule: NormalRule) =
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))

  def invalid(rule: NormalRule) =
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))

  def posValid(rule: NormalRule) =
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

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom,Status,String)]()

    try {
      updateStrategy match {
        case `UpdateStrategyDoyle` => updateDoyle(atoms)
        case `UpdateStrategyStepwise` => updateStepwise(atoms)
      }
      checkTmsSemantics()
      checkSelfSupport()
      checkConsistency()
    } catch {
      case e:IncrementalUpdateFailureException => {
        invalidateModel()
      }
    }

  }

  def invalidateModel(): Unit = {
    atomsNeedingSupp foreach setUnknown
  }

  def updateDoyle(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown //Marking the nodes
    atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
    atoms foreach fixAndPropagateStatus // Relaxing circularities (might lead to contradictions)
  }

  def updateStepwise(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown
    var lastAtom: Option[Atom] = None
    while (hasUnknown) {
      unknownAtoms foreach determineAndPropagateStatus
      val atom = getOptUnknownOtherThan(lastAtom) //ensure that the same atom is not tried consecutively
      if (atom.isDefined) {
        fixAndDetermineAndPropagateStatus(atom.get)
      }
      lastAtom = atom
    }
  }

  def getOptUnknownOtherThan(atom: Option[Atom]): Option[Atom] = {

    val atoms = unknownAtoms

    if (atoms.isEmpty) return None
    if (atoms.size == 1) return Some(atoms.head)

    if (doForceChoiceOrder) {
      val maybeAtom: Option[Atom] = forcedChoiceSeq find (status(_) == unknown)
      if (maybeAtom.isDefined) {
        return maybeAtom
      }
    }

    val list = List[Atom]() ++ atoms
    val idx = if (shuffle) { util.Random.nextInt(list.size) } else 0
    val elem = list(idx)

    if (atom == None) return Some(elem)
    val elemToAvoid = atom.get
    if (elem != elemToAvoid) return Some(elem)
    return list find (_ != elemToAvoid)
  }

  def setIn(rule: NormalRule) = {
    status(rule.head) = in
    supp(rule.head) = Set() ++ rule.body
    suppRule(rule.head) = Some(rule)

    if (recordStatusSeq) statusSeq = statusSeq :+ (rule.head,in,"set")
  }

  //return success
  def setOut(a: Atom) = {
    status(a) = out
    if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"set")
    //supp(a) = Set() ++ (justifications(a) map (findSpoiler(_).get)) //TODO write-up missing
    val maybeAtoms: List[Option[Atom]] = justifications(a) map (findSpoiler(_))
    if (maybeAtoms exists (_.isEmpty)) {
      throw new IncrementalUpdateFailureException()
    }
    supp(a) = Set() ++ maybeAtoms map (_.get)
    suppRule(a) = None
  }

  def setUnknown(a: Atom) = {
    status(a) = unknown
    supp(a) = Set()
    suppRule(a) = None
    //if (recordStatusSeq) statusSeq = statusSeq :+ (a,unknown,"set")
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
      val aff = affected(a) + a //TODO no test coverage
      aff foreach setUnknown
      aff foreach fixAndPropagateStatus
    }
  }

  def fixAndDetermineAndPropagateStatus(a: Atom): Unit = {
    if (status(a) != unknown)
      return

    if (fix(a)) {
      unknownCons(a) foreach determineAndPropagateStatus
    } else {
      affected(a) foreach setUnknown //TODO no test coverage
    }
  }

  def fix(a: Atom): Boolean = {

    if (recordChoiceSeq) choiceSeq = choiceSeq :+ a

    justifications(a) find posValid match {
      case Some(rule) => {
        if (affected(a).isEmpty) fixIn(rule)
        else return false
      }
      case None => fixOut(a)
    }
    true

  }

  def fixIn(rulePosValid: NormalRule): Unit = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rulePosValid.head,in,"fix")
    setIn(rulePosValid)
    rulePosValid.neg filter (status(_) == unknown) foreach fixOut //fix ancestors
    /* not that setIn here has to be called first. consider
       a :- not b.
       b :- not a. ,
       where the choice is for status(a)=in. then, this status needs to be available
       when the spoiler for rule b :- not a is sought.
     */
  }

  def fixOut(a: Atom): Unit = {
    status(a) = out
    if (recordStatusSeq) statusSeq = statusSeq :+ (a,out,"fix")
    val maybeAtoms: List[Option[Atom]] = openJustifications(a) map { r => (r.pos find (status(_)==unknown)) }
    val unknownPosAtoms = (maybeAtoms filter (_.isDefined)) map (_.get)
    unknownPosAtoms foreach fixOut //fix ancestors
    //note that only positive body atoms are used to create a spoilers, since a rule with an empty body
    //where the negative body is out/unknown is
    setOut(a)
  }

  def openJustifications(a: Atom) = justifications(a) filter (!invalid(_))

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
    rules = rules filter (_ != rule)
    val remainingAtoms = allAtoms()
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
    suppRule remove a
  }

  def checkTmsSemantics(): Unit = {
    if (!doTmsSemanticsCheck) return
    if (atomsNeedingSupp exists (supp(_).isEmpty)) {
      throw new RuntimeException("no support for atoms "+(atomsNeedingSupp filter (supp(_).isEmpty)))
    }
  }

  def checkSelfSupport(): Unit = {
    if (!doSelfSupportCheck) return
    if (inAtoms exists unfoundedSelfSupport) {
      invalidateModel()
    }
  }

  def checkConsistency(): Unit = {
    if (!doConsistencyCheck) return
    if ((inAtoms diff facts) exists (a => !(justifications(a) exists valid))) {
      invalidateModel()
    }
  }

  def selfSupport(a:Atom): Boolean = supp(a) contains a

  def unfoundedSelfSupport(a: Atom): Boolean = {
    if (!selfSupport(a)) return false
    justifications(a) filter valid exists (r => !(r.pos contains a))
  }

  private var doForceChoiceOrder = false
  var forcedChoiceSeq = Seq[Atom]()
  def forceChoiceOrder(seq: Seq[Atom]) = {
    doForceChoiceOrder=true
    forcedChoiceSeq = seq
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