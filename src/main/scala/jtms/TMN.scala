package jtms

import core.EvaluationResult.Model
import core._

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map}

object TMN {
  def apply(P: Program) = {
    val tmn = new TMN(P.atoms)

    P.rules.foreach(tmn.add)

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
  for (j <- rules) {
    for (m <- j.body) {
      Cons(m) += j.head
    }
  }

  def init(a: Atom) = {
    if (!status.isDefinedAt(a)) status(a) = out
    if (!Cons.isDefinedAt(a)) Cons(a) = Set[Atom]()
    if (!Supp.isDefinedAt(a)) Supp(a) = Set[Atom]()
    if (!SuppRule.isDefinedAt(a)) SuppRule(a) = None
  }

  /** @return true if M is admissible **/
  def set(M: Set[Atom]): Boolean = {
    val m = M.toList
    for (i <- 0 to M.size - 1) {
      val j: Option[Rule] = findSuppRule(m, i)
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

  def getModel(): Option[SingleModel] = {
    val inAtoms = status.filter(_._2 == in)
    if (inAtoms.nonEmpty)
      return Some(SingleModel(inAtoms.keys.toSet))

    None
  }

  def isFounded(atoms: Model): Boolean = {
    if (atoms.forall(status(_) == in)) {

      val suppWithAtom = atoms.filter(atom => SuppTrans(atom) contains atom)

      if (suppWithAtom.nonEmpty) {
        return suppWithAtom.forall(atom => suppRules(AConsTrans(atom)).forall(_.neg.nonEmpty))
      }

      return true
    }

    return false
  }

  /** takes atoms at list M index idx and tries to find a valid rule
    * that is founded wrt indexes 0..idx-1
    */
  def findSuppRule(M: List[Atom], idx: Int): Option[Rule] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    val rules = rulesWithHead(n).filter(j => j.pos.subsetOf(MSub) && j.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  //TMS update algorithm
  def add(rule: Rule): Set[Atom] = {

    val head = rule.head //alias

    rules += rule
    rule.body.foreach(Cons(_) += head)

    init(head)

    //if conclusion was already drawn, we are done
    if (status(head) == in || isInvalid) {
      return scala.collection.immutable.Set()
    }
    def isInvalid(): Boolean = {
      //TODO (HB) isValid vs (un)foundedInvalid later
      val spoiler: Option[Atom] = findSpoiler(rule)
      if (spoiler.isDefined) {
        Supp(head) += spoiler.get
        return true
      }
      false
    }

    //we now know that the rule is valid in M
    if (ACons(head).isEmpty) {
      //then we can treat head independently
      setIn(rule)
      checkForDDB //TODO (HB): how come this step is necessary, if rule.head is independent?
      return scala.collection.immutable.Set()
    }

    val L = AffectedAtoms(head)

    update(L)
  }

  def update(atoms: Set[Atom]): Set[Atom] = {

    def stateOfAtoms() = atoms.map(a => (a, status(a))).toList

    val oldState = stateOfAtoms


    atoms.foreach(setUnknown)
    atoms.foreach(setConsequences)
    atoms.foreach(chooseAssignments)

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
    val model = getModel


    model match {
      case Some(SingleModel(nodes)) => nodes.foreach(n => {
        if (n == Falsum || Ncontr.contains(n) && status(n) == in)
          DDB(n)
      })
      case None =>
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

    val r_contr = suppRules(assumptions.map(_.head))

    val pos_contr = r_contr.flatMap(_.pos)
    val neg_contr = r_contr.flatMap(_.neg) - n_star;

    val rule = new RuleFromBacktracking(pos_contr, neg_contr, n_star)

    add(rule)
  }

  def suppRules(atoms: Set[Atom]) = {
    SuppRule.filterKeys(atoms.contains(_))
      .values
      .filter(_.isDefined)
      .map(_.get)
      .toSet
  }

  def rulesWithHead(h: Atom) = rules.filter(_.head == h)

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

    if (a == Falsum || Ncontr.contains(a)) {
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
    Supp(n) = rulesWithHead(n).map(findSpoiler(_).get)
    SuppRule(n) = None
  }

  def findSpoiler(rule: Rule): Option[Atom] = {
    if (math.random < 0.5) {
      val opt = selectAtom(rule.pos.filter(status(_) == out))
      if (opt.isDefined) {
        return opt
      } else {
        return selectAtom(rule.neg.filter(status(_) == in))
      }
    } else {
      val opt = selectAtom(rule.neg.filter(status(_) == in))
      if (opt.isDefined) {
        return opt
      } else {
        return selectAtom(rule.pos.filter(status(_) == out))
      }
    }
  }

  def setUnknown(atom: Atom): Unit = {
    status(atom) = unknown
    Supp(atom) = Set()
    SuppRule(atom) = None
  }

  def setConsequences(a: Atom): Unit = {
    if (status(a) == unknown) {
      val rn = rulesWithHead(a)
      val rule: Option[Rule] = selectRule(rn.filter(foundedValid))
      if (rule.isDefined) {
        setIn(rule.get)
        unknownCons(a).foreach(setConsequences)
      } else if (rn.forall(foundedInvalid)) {
        setOut(a)
        unknownCons(a).foreach(setConsequences)
      }
    }
  }

  def chooseAssignments(a: Atom): Unit = {
    if (status(a) == unknown) {
      val rules = rulesWithHead(a)
      val rule: Option[Rule] = selectRule(rules.filter(unfoundedValid))
      if (rule.isDefined) {
        val aCons = ACons(a)
        if (aCons.isEmpty) {
          setIn(rule.get)
          rule.get.neg.filter(status(_) == unknown).foreach(status(_) = out)
          unknownCons(a).foreach(chooseAssignments)
        } else {
          for (x <- (aCons + a)) {
            status(x) = unknown
            chooseAssignments(x)
          }
        }
      } else {
        //all rules are unfounded invalid. in particular, for every rule in rules, some atom in rule.pos is unknown
        status(a) = out
        for (h <- rules) {
          val x = selectAtom(h.pos.filter(status(_) == unknown))
          // TODO: this might be needed because of the non existing ordering
          // We usually can expect to always have a rule (if ordering is correct)
          x.foreach(status(_) = out)
        }
        setOut(a)
        unknownCons(a).foreach(chooseAssignments)
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

  def foundedValid(rule: Rule): Boolean = {
    rule.pos.forall(status(_) == in) && rule.neg.forall(status(_) == out)
  }

  def foundedInvalid(rule: Rule): Boolean = {
    rule.pos.exists(status(_) == out) || rule.neg.exists(status(_) == in)
  }

  def unfoundedValid(rule: Rule): Boolean = {
    rule.pos.forall(status(_) == in) && !rule.neg.exists(status(_) == in) //&& j.O.exists(status(_)==unknown)
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