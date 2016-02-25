package jtms

import core._

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map}

/**
  * truth maintenance network
  * Created by hb on 12/22/15.
  */
class TMN(var N: collection.immutable.Set[Atom], var J: Set[Rule] = Set()) {

  val Cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val Supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val SJ: Map[Atom, Option[Rule]] = new HashMap[Atom, Option[Rule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status]

  for (a <- N) {
    init(a)
  }
  for (j <- J) {
    for (m <- j.I union j.O) {
      Cons(m) += j.head
    }
  }

  def init(a: Atom) = {
    if (!status.isDefinedAt(a)) status(a) = out
    if (!Cons.isDefinedAt(a)) Cons(a) = Set[Atom]()
    if (!Supp.isDefinedAt(a)) Supp(a) = Set[Atom]()
    if (!SJ.isDefinedAt(a)) SJ(a) = None
  }

  /** @return true if M is admissible **/
  def set(M: Set[Atom]): Boolean = {
    val m = M.toList
    for (i <- 0 to M.size - 1) {
      val j: Option[Rule] = findSJ(m, i)
      if (j.isEmpty) {
        return false
      }
      setIn(j.get)
    }
    for (n <- N.diff(M.toSet)) {
      setOut(n)
    }
    true
  }

  def getModel() = {
    status.filter(_._2 == in).map(_._1).toSet
  }

  /** takes atoms at list M index idx and tries to find a valid rule
    * that is founded wrt indexes 0..idx-1
    */
  def findSJ(M: List[Atom], idx: Int): Option[Rule] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    val rules = Jn(n).filter(j => j.I.subsetOf(MSub) && j.O.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  //TMS update algorithm
  def add(j: Rule): Set[Atom] = {

    val head = j.head //alias

    //update structure
    J += j
    for (m <- j.I union j.O) {
      Cons(m) += head
    }

    init(head)

    //if conclusion was already drawn, we are done
    if (status(head) == in) {
      return scala.collection.immutable.Set()
    }

    //otherwise, we are done, if the new rules is not valid in M, i.e.,
    //head does not need to be concluded
    val spoiler: Option[Atom] = findSpoiler(j)
    if (spoiler.isDefined) {
      Supp(head) += spoiler.get
      return scala.collection.immutable.Set()
    }

    if (ACons(head).isEmpty) {
      //then we can treat head independently
      setIn(j)
      checkForDDB
      // TODO (CF): Missing to add head to M (M = M + head)?
      return scala.collection.immutable.Set()
    }

    val L = AffectedAtoms(head)

    update(L)
  }

  def update(L: Set[Atom]): Set[Atom] = {

    def stateOfAtoms() = L.map(n => (n, status(n))).toList

    val oldState = stateOfAtoms

    setUnknown(L)

    setConsequences(L)

    chooseAssignments(L)

    checkForDDB

    val newState = stateOfAtoms

    val diffState = oldState.diff(newState)

    diffState.map(_._1).toSet
  }

  def remove(rule: Rule) = {

    val head = rule.head

    val rulesFromBacktracking = J.filter(_.isInstanceOf[RuleFromBacktracking])

    var L = AffectedAtoms(head) ++ rulesFromBacktracking.flatMap(x => AffectedAtoms(x.head))

    def removeRule(j: Rule) = {
      for (m <- j.I union j.O) {
        Cons(m) -= j.head
      }

      J -= j
    }

    removeRule(rule)

    rulesFromBacktracking.foreach(removeRule)

    // if no other rule exists containing the atom - remove it completely
    if (!J.exists(_.atoms.contains(head))) {
      N -= head
      status.remove(head)
      Supp.remove(head)
      SJ.remove(head)
      Cons.remove(head)
      L -= head
    }

    this.update(L)
  }

  def checkForDDB() = {
    val model = getModel()

    for (n <- model) {
      if (Ncont.contains(n) && status(n) == in)
        DDB(n)
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
    val n_star = selectAtom(n_a.O).get

    val j_cont = Jn(assumptions.map(_.head))

    val I_cont = j_cont.flatMap(_.I)
    val O_cont = j_cont.flatMap(_.O) - n_star;

    val rule = new RuleFromBacktracking(I_cont, O_cont, n_star)

    add(rule)
  }

  def Jn(atoms: Set[Atom]) = {
    SJ.filterKeys(atoms.contains(_)).values.map(_.get).toSet
  }

  def Jn(n: Atom) = J.filter(_.head == n)

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

  def MaxAssumptions(n: Atom): Set[Rule] = {

    def asAssumption(assumption: Atom) = SJ(assumption).filterNot(_.O.isEmpty)

    if (Ncont.contains(n)) {
      val assumptionsOfN = AntTrans(n).map(asAssumption).filter(_.isDefined).map(_.get)

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

  def setIn(j: Rule) = {
    status(j.head) = in
    Supp(j.head) = j.I union j.O
    SJ(j.head) = Option(j)
  }

  def setOut(n: Atom) = {
    status(n) = out
    Supp(n) = Jn(n).map(findSpoiler(_).get)
    SJ(n) = None
  }

  def findSpoiler(j: Rule): Option[Atom] = {
    if (math.random < 0.5) {
      val opt = selectAtom(j.I.filter(status(_) == out))
      if (opt.isDefined) {
        return opt
      } else {
        return selectAtom(j.O.filter(status(_) == in))
      }
    } else {
      val opt = selectAtom(j.O.filter(status(_) == in))
      if (opt.isDefined) {
        return opt
      } else {
        return selectAtom(j.I.filter(status(_) == out))
      }
    }
  }

  def setUnknown(atom: Atom): Unit = {
    status(atom) = unknown
    Supp(atom) = Set()
    SJ(atom) = None
  }

  def setUnknown(L: Set[Atom]): Unit = L.foreach(setUnknown(_))

  def setConsequences(L: Set[Atom]): Unit = {
    for (n <- L) {
      setConsequences(n)
    }
  }

  def setConsequences(a: Atom): Unit = {
    if (status(a) == unknown) {
      val jn = Jn(a)
      val j: Option[Rule] = selectRule(jn.filter(foundedValid))
      if (j.isDefined) {
        setIn(j.get)
        setConsequences(unknownCons(a))
      } else if (jn.forall(foundedInvalid)) {
        setOut(a)
        setConsequences(unknownCons(a))
      }
    }
  }

  def chooseAssignments(L: Set[Atom]): Unit = {
    for (n <- L) {
      chooseAssignments(n)
    }
  }

  def chooseAssignments(a: Atom): Unit = {
    if (status(a) == unknown) {
      val jn = Jn(a)
      val j: Option[Rule] = selectRule(jn.filter(unfoundedValid))
      if (j.isDefined) {
        val aCons = ACons(a)
        if (aCons.isEmpty) {
          setIn(j.get)
          j.get.O.filter(status(_) == unknown).foreach(status(_) = out)
          chooseAssignments(unknownCons(a))
        } else {
          for (m <- (aCons + a)) {
            status(m) = unknown
            chooseAssignments(m)
          }
        }
      } else {
        //all jn are unfounded invalid. in particular, for every j in jn, some atom in j.I is unknown
        status(a) = out
        for (h <- jn) {
          val m = selectAtom(h.I.filter(status(_) == unknown))
          // TODO: this might be needed because of the non existing ordering
          // We usually can expect to always have a rule (if ordering is correct)
          m.foreach(status(_) = out)
        }
        setOut(a)
        chooseAssignments(unknownCons(a))
      }
    }
  }

  def Ncont = N.filter(_.isInstanceOf[ContradictionAtom])

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
    r.I.forall(status(_) == in) && r.O.forall(status(_) == out)
  }

  def foundedInvalid(r: Rule): Boolean = {
    r.I.exists(status(_) == out) || r.O.exists(status(_) == in)
  }

  def unfoundedValid(r: Rule): Boolean = {
    r.I.forall(status(_) == in) && !r.O.exists(status(_) == in) //&& j.O.exists(status(_)==unknown)
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