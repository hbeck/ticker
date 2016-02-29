package jtms

import core._

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map}

object TMN {
  def apply(P: Program) = {
    val tmn = new TMN(P.atoms)

    P.rules.foreach(tmn.add) //TODO (HB) think about order

    tmn
  }
}

/**
  * truth maintenance network
  * Created by hb on 12/22/15.
  */
class TMN(var N: collection.immutable.Set[Atom], var rules: Set[Rule] = Set()) {

  val Cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val Supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val SuppRule: Map[Atom, Option[Rule]] = new HashMap[Atom, Option[Rule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status]

  for (a <- N) {
    init(a)
  }
  for (rule <- rules) {
    for (m <- rule.body) {
      Cons(m) += rule.head
    }
  }

  def init(a: Atom) = {
    if (!status.isDefinedAt(a)) status(a) = out
    if (!Cons.isDefinedAt(a)) Cons(a) = Set[Atom]()
    if (!Supp.isDefinedAt(a)) Supp(a) = Set[Atom]()
    if (!SuppRule.isDefinedAt(a)) SuppRule(a) = None
  }

  /** @return true if M is admissible **/
  def set(M: Set[Atom]): Boolean = { //TODO (HB) Set vs List. Always list for order?
    val m = M.toList
    for (i <- 0 to M.size - 1) {
      val rule: Option[Rule] = findSuppRule(m, i)
      if (rule.isEmpty) {
        return false
      }
      setIn(rule.get)
    }
    for (n <- N diff M) {
      setOut(n)
    }
    true
  }

  def getModel() = {
    //status.filter(_._2 == in).map(_._1).toSet
    status.filter(_._2 == in).keys.toSet //TODO (HB) review (in prtcl: .keySet doesnt work)
  }

  /** takes atoms at list M index idx and tries to find a valid rule
    * that is founded wrt indexes 0..idx-1
    */
  def findSuppRule(M: List[Atom], idx: Int): Option[Rule] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    val rules = Rn(n).filter(rule => rule.pos.subsetOf(MSub) && rule.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  //TMS update algorithm
  def add(rule: Rule): Set[Atom] = {

    val head = rule.head //alias

    //update structure
    rules += rule
    for (m <- rule.body) {
      Cons(m) += head
    }

    init(head)

    //if conclusion was already drawn, we are done
    if (status(head) == in) {
      return scala.collection.immutable.Set()
    }

    //otherwise, we are done, if the new rule is not valid in M
    //because then the head does not need to be concluded
    val spoiler: Option[Atom] = findSpoiler(rule)
    if (spoiler.isDefined) {
      Supp(head) += spoiler.get
      return scala.collection.immutable.Set()
    }

    //we now know that the rule is valid in M
    if (ACons(head).isEmpty) {
      //then we can treat head independently
      setIn(rule)
      checkForDDB //TODO (HB): how come this step is necessary, if rule.head is independent?
      // TODO (CF): Missing to add head to M (M = M + head)?
      return scala.collection.immutable.Set()
    }

    val L = AffectedAtoms(head)

    update(L)
  }

  def update(atoms: Set[Atom]): Set[Atom] = {

    def stateOfAtoms() = atoms.map(a => (a, status(a))).toList

    val oldState = stateOfAtoms

    setUnknown(atoms)

    setConsequences(atoms)

    chooseAssignments(atoms)

    checkForDDB

    val newState = stateOfAtoms

    val diffState = oldState.diff(newState)

    diffState.map(_._1).toSet
  }

  def remove(rule: Rule) = {

    val head = rule.head

    val rulesFromBacktracking = rules.filter(_.isInstanceOf[RuleFromBacktracking])

    var L = AffectedAtoms(head) ++ rulesFromBacktracking.flatMap(x => AffectedAtoms(x.head))

    def removeRule(rule: Rule) = {
      for (m <- rule.body) {
        Cons(m) -= rule.head
      }

      rules -= rule
    }

    removeRule(rule)

    rulesFromBacktracking.foreach(removeRule)

    // if no other rule exists containing the atom - remove it completely
    if (!rules.exists(_.atoms.contains(head))) {
      N -= head
      status.remove(head)
      Supp.remove(head)
      SuppRule.remove(head)
      Cons.remove(head)
      L -= head
    }

    this.update(L)
  }

  def checkForDDB() = {
    val model = getModel()

    for (a <- model) {
      if (Ncontr.contains(a) && status(a) == in)
        DDB(a)
    }
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

    val r_contr = Rn(assumptions.map(_.head))

    val pos_contr = r_contr.flatMap(_.pos)
    val neg_contr = r_contr.flatMap(_.neg) - n_star;

    val rule = new RuleFromBacktracking(pos_contr, neg_contr, n_star)

    add(rule)
  }

  def Rn(atoms: Set[Atom]) = {
    SuppRule.filterKeys(atoms.contains(_)).values.map(_.get).toSet
  }

  def Rn(n: Atom) = rules.filter(_.head == n)

  //ACons(n) = {x ∈ Cons(n) | n ∈ Supp(x)}
  def ACons(n: Atom): Set[Atom] = Cons(n).filter(Supp(_).contains(n))

  def AConsTrans(n: Atom) = trans(ACons, n)

  def SuppTrans(n: Atom) = trans(Supp, n)

  def Ant(n: Atom): Set[Atom] = {
    if (status(n) == in)
      return Supp(n)
    return Set()
  }

  def AntTrans(a: Atom) = trans(Ant, a)

  def AffectedAtoms(a: Atom) = AConsTrans(a) + a

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

  def setIn(rule: Rule) = {
    status(rule.head) = in
    Supp(rule.head) = rule.body
    SuppRule(rule.head) = Option(rule)
  }

  def setOut(n: Atom) = {
    status(n) = out
    Supp(n) = Rn(n).map(findSpoiler(_).get)
    SuppRule(n) = None
  }

  def findSpoiler(j: Rule): Option[Atom] = {
    if (math.random < 0.5) {
      val opt = selectAtom(j.pos.filter(status(_) == out))
      if (opt.isDefined) {
        return opt
      } else {
        return selectAtom(j.neg.filter(status(_) == in))
      }
    } else {
      val opt = selectAtom(j.neg.filter(status(_) == in))
      if (opt.isDefined) {
        return opt
      } else {
        return selectAtom(j.pos.filter(status(_) == out))
      }
    }
  }

  def setUnknown(atom: Atom): Unit = {
    status(atom) = unknown
    Supp(atom) = Set()
    SuppRule(atom) = None
  }

  def setUnknown(atoms: Set[Atom]): Unit = atoms.foreach(setUnknown(_))

  def setConsequences(atoms: Set[Atom]): Unit = {
    for (a <- atoms) {
      setConsequences(a)
    }
  }

  def setConsequences(a: Atom): Unit = {
    if (status(a) == unknown) {
      val rn = Rn(a)
      val rule: Option[Rule] = selectRule(rn.filter(foundedValid))
      if (rule.isDefined) {
        setIn(rule.get)
        setConsequences(unknownCons(a))
      } else if (rn.forall(foundedInvalid)) {
        setOut(a)
        setConsequences(unknownCons(a))
      }
    }
  }

  def chooseAssignments(atoms: Set[Atom]): Unit = {
    for (a <- atoms) {
      chooseAssignments(a)
    }
  }

  def chooseAssignments(a: Atom): Unit = {
    if (status(a) == unknown) {
      val rn = Rn(a)
      val rule: Option[Rule] = selectRule(rn.filter(unfoundedValid))
      if (rule.isDefined) {
        val aCons = ACons(a)
        if (aCons.isEmpty) {
          setIn(rule.get)
          rule.get.neg.filter(status(_) == unknown).foreach(status(_) = out)
          chooseAssignments(unknownCons(a))
        } else {
          for (x <- (aCons + a)) {
            status(x) = unknown
            chooseAssignments(x)
          }
        }
      } else {
        //all rn are unfounded invalid. in particular, for every rule in rn, some atom in rule.I is unknown
        status(a) = out
        for (h <- rn) {
          val x = selectAtom(h.pos.filter(status(_) == unknown))
          // TODO: this might be needed because of the non existing ordering
          // We usually can expect to always have a rule (if ordering is correct)
          x.foreach(status(_) = out)
        }
        setOut(a)
        chooseAssignments(unknownCons(a))
      }
    }
  }

  def Ncontr = N.filter(_.isInstanceOf[ContradictionAtom])

  def unknownCons(a: Atom) = Cons(a).filter(status(_) == unknown)

  def selectAtom(atoms: Set[Atom]): Option[Atom] = {
    if (atoms.isEmpty)
      return None

    //TODO what about the ordering?
    Some(atoms.head)
  }

  def selectRule(rules: Set[Rule]): Option[Rule] = {
    if (rules.isEmpty)
      return None

    //TODO what about the ordering?
    Some(rules.head)
  }

  def foundedValid(r: Rule): Boolean = {
    r.pos.forall(status(_) == in) && r.neg.forall(status(_) == out)
  }

  def foundedInvalid(r: Rule): Boolean = {
    r.pos.exists(status(_) == out) || r.neg.exists(status(_) == in)
  }

  def unfoundedValid(r: Rule): Boolean = {
    r.pos.forall(status(_) == in) && !r.neg.exists(status(_) == in) //&& j.O.exists(status(_)==unknown)
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

}