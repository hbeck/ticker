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

  //var atoms: Set[Atom]= Set()
  var rules: Set[Rule]= Set()

  val Cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val Supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]] //TODO pos vs neg
  val SuppRule: Map[Atom, Option[Rule]] = new HashMap[Atom, Option[Rule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status] //at least 'in' consequence of SuppRule

  def registerAtom(a: Atom): Unit = {
    //atoms += a
    if (!status.isDefinedAt(a)) status(a) = out
    if (!Cons.isDefinedAt(a)) Cons(a) = Set[Atom]()
    if (!Supp.isDefinedAt(a)) Supp(a) = Set[Atom]()
    if (!SuppRule.isDefinedAt(a)) SuppRule(a) = None
  }

  def atoms() = Cons.keySet

  //TODO (HB) case 'None'
  def getModel() = {
    status.filter(_._2 == in).keys.toSet
  }

  //TMS update algorithm
  def add(rule: Rule): collection.immutable.Set[Atom] = {

    rules += rule
    rule.atoms foreach registerAtom
    rule.body foreach (Cons(_) += rule.head)

    if (status(rule.head) == in || isInvalid(rule)) {
      return collection.immutable.Set()
    }

    //Updating of beliefs required
    //(rule is valid, head needs to be concluded)
    val affected = AConsTrans(rule.head) //TODO vs ACons

    if (affected.isEmpty) {
      //then we can treat head independently
      setIn(rule)
      checkForDDB //needed if rule.head is a contradiction node
      return collection.immutable.Set(rule.head)
    }

    update(affected + rule.head)

  }

  def update(atoms: Set[Atom]): collection.immutable.Set[Atom] = {

    def stateOfAtoms() = atoms map (a => (a, status(a))) toList

    val oldState = stateOfAtoms

    atoms foreach setUnknown //Marking the nodes
    atoms foreach determineAndPropagateStatus // Evaluating the nodes' justifications
    atoms foreach fixAndPropagateStatus // Relaxing circularities

    checkForDDB

    val newState = stateOfAtoms
    (oldState diff newState) map (_._1) toSet
  }

  def isInvalid(rule: Rule): Boolean = { //TODO
    findSpoiler(rule) match {
      case Some(spoiler) => {
        Supp(rule.head) += spoiler
        return true
      }
      case None => return false
    }
  }

  def checkForDDB() = {
    val model = getModel()
    for (a <- model) {
      if ((Ncontr contains a) && status(a) == in)
        DDB(a)
    }
  }

  def Ncontr = atoms filter (_.isInstanceOf[ContradictionAtom])

  def rulesWithHead(h: Atom) = rules.filter(_.head == h)

  //ACons(n) = {x ∈ Cons(n) | n ∈ Supp(x)}
  def ACons(n: Atom): Set[Atom] = Cons(n).filter(Supp(_).contains(n))

  def AConsTrans(n: Atom) = trans(ACons, n)

  def SuppTrans(n: Atom) = trans(Supp, n)

  //TODO
  def setIn(rule: Rule) = {
    status(rule.head) = in
    Supp(rule.head) = Set() ++ rule.body
    SuppRule(rule.head) = Option(rule)
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

    val rules = rulesWithHead(a)

    rules find unfoundedValid match {
      case Some(rule) => {
        val affected = ACons(a)
        if (affected.isEmpty) {
          fixIn(rule)
          unknownCons(a) foreach fixAndPropagateStatus
        } else {
          for (x <- (affected + a)) {
            setUnknown(x)
            fixAndPropagateStatus(x)
          }
          //vs TODO (HB) - which is right?
//          (affected + a) foreach setUnknown
//          (affected + a) foreach fixAndPropagateStatus
        }
      }
      case None => {
        //all rules are unfounded invalid. for every rule in rules, some atom in rule.pos is unknown.
        //must set (common) rule head out. to justify this, must create a support first, i.e.,
        //for every rule take a positive atom that is unknown and set it to false
        fixOut(a)
        unknownCons(a) foreach fixAndPropagateStatus
      }
    }
  }

  def fixIn(unfoundedValidRule: Rule) = {
    unfoundedValidRule.neg filter (status(_) == unknown) foreach setOut
    setIn(unfoundedValidRule)
  }

  def fixOut(a: Atom) = {
    val unknownPosAtoms = rulesWithHead(a) map { r => (r.pos find (status(_)==unknown)).get }
    unknownPosAtoms foreach setOut
    //
    setOut(a)
  }

  def unknownCons(a: Atom) = Cons(a).filter(status(_) == unknown)

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

  def DDB(a: Atom) = {
    val assumptions = MaxAssumptions(a)

    if (assumptions.isEmpty)
      throw new RuntimeException("We have an unsolvable contradiction for atom " + a)

    // TODO: Ordering + Selection?
    val n_a = selectRule(assumptions).get

    // TODO: Ordering + Selection?
    // (we pick currently only the first O)
    val n_star = selectAtom(n_a.neg).get

    val r_contr = suppRules(assumptions.map(_.head))

    val pos_contr = r_contr.flatMap(_.pos)
    val neg_contr = r_contr.flatMap(_.neg) - n_star;

    val rule = new RuleFromBacktracking(pos_contr, neg_contr, n_star)

    add(rule)
  }

  def MaxAssumptions(a: Atom): Set[Rule] = {

    def asAssumption(assumption: Atom) = SuppRule(assumption).filterNot(_.neg.isEmpty)

    if (Ncontr.contains(a)) {
      val assumptionsOfN = AntTrans(a).map(asAssumption).filter(_.isDefined).map(_.get)

      val assumptions = assumptionsOfN
        .filter(a => {
          val otherAssumptions = assumptionsOfN - a

          val allOtherAssumptions = otherAssumptions.flatMap(x => AntTrans(x.head))

          !allOtherAssumptions.contains(a.head)
        })

      return assumptions
    }

    Set()
  }

  def suppRules(atoms: Set[Atom]) = {
    SuppRule.filterKeys(atoms.contains(_)).values.map(_.get).toSet
  }

  def remove(rule: Rule) = {

    val head = rule.head

    val rulesFromBacktracking = rules.filter(_.isInstanceOf[RuleFromBacktracking])

    def affectedAtoms(a: Atom) = AConsTrans(a) + a

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

    this.update(L)
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

  def Ant(n: Atom): Set[Atom] = {
    if (status(n) == in) return Supp(n)
    Set()
  }

  def AntTrans(a: Atom) = trans(Ant, a)

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

}