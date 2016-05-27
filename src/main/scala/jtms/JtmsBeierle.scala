package jtms

import core._
import core.asp.{AspRuleFromBacktracking, NormalProgram, NormalRule}

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, Map, Set}

object JtmsBeierle {

  def apply(P: NormalProgram): JtmsBeierle = {
    val tmn = new JtmsBeierle()
    P.rules foreach tmn.add
    tmn
  }

}

/**
  * justification-based truth maintenance network
  *
  * follows quite closely the presentation in the
  * book chapter from Beierle and Kern-Isberner
  *
  * Created by hb on 12/22/15; 03/25/16
  */
case class JtmsBeierle() {

  var rules: List[NormalRule] = List()

  val Cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val Supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  val SuppRule: Map[Atom, Option[NormalRule]] = new HashMap[Atom, Option[NormalRule]]
  val status: Map[Atom, Status] = new HashMap[Atom, Status] //at least 'in' consequence of SuppRule

  def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None
    Some(atoms.toSet)
  }

  def atoms() = Cons.keySet

  def inAtoms() = status.keys filter (status(_) == in)

  def contradictionAtom(a: Atom) = a.isInstanceOf[ContradictionAtom]

  def add(rule: NormalRule): Unit = {
    updateSteps1to5(rule)
    //6
    for (n <- atoms()) {
      if (contradictionAtom(n) && status(n) == in) {
        DDBBeierleOriginal(n) //there is no need to continue iteration after first unsolvable contradiction [!]
      }
    }
    //7 would just concern returning the diff (omitted here)
  }

  def updateSteps1to5(rule: NormalRule): Unit = {
    //1
    register(rule)
    if (status(rule.head) == in) return
    if (invalid(rule)) return
    //2
    if (ACons(rule.head).isEmpty) {
      setIn(rule)
      return
    }
    //3
    val L = repercussions(rule.head) + rule.head
    for (atom <- L) {
      status(atom) = unknown //vs setUnknown [!]
    }
    //4 determine status
    for (atom <- L) {
      step4a(atom)
    }
    //5 fix (choose) status
    for (atom <- L) {
      step5a(atom)
    }
  }

  //determine status
  def step4a(atom: Atom): Unit = {
    if (status(atom) != unknown)
      return

    justifications(atom) find foundedValid match {
      case Some(rule) => {
        setIn(rule)
        val unk = Cons(atom) filter (status(_) == unknown)
        for (u <- unk){
          step4a(u)
        }
      }
      case None => {
        if (justifications(atom) forall foundedInvalid) {
          setOut(atom)
          val unk = Cons(atom) filter (status(_) == unknown)
          for (u <- unk){
            step4a(u)
          }
        }
      }
    }
  }

  //fix (choose) status
  def step5a(atom: Atom): Unit = {
    if (status(atom) != unknown)
      return

    justifications(atom) find unfoundedValid match {
      case Some(rule) => {
        if (!ACons(atom).isEmpty) {
          for (n <- ACons(atom) + atom) {
            status(n) = unknown //vs setUnknown [!]
            step5a(n) //vs first setting all unknown, and only then call 5a if still necessary [!] (see * below)
          }
        } else {
          setIn(rule)
          for (n <- rule.neg) {
            if (status(n) == unknown) {
              status(n) = out //vs setOutOriginal [!]
            }
          }
          val unk = Cons(atom) filter (status(_) == unknown) //* here other variant is chosen. deliberately? [1]
          for (u <- unk){
            step5a(u)
          }
        }
      }
      case None => { //all justifications(atom) are unfounded invalid
        //do status(atom)=out as part of setOut below
        for (rule <- justifications(atom)) {
          val n: Option[Atom] = rule.pos find (status(_) == unknown) //in general, rule.pos might be empty! [!]
          if (n.isEmpty) {
            throw new RuntimeException("did not find rule.pos atom with status unknown in rule "+rule+" for atom "+atom)
          }
        }
        setOut(atom)
        val unk = Cons(atom) filter (status(_) == unknown)
        for (u <- unk){
          step5a(u)
        }
      }
    }
  }

  def setIn(rule: NormalRule) = {
    status(rule.head) = in
    Supp(rule.head) = Set() ++ rule.body
    SuppRule(rule.head) = Some(rule)
  }

  def setOut(atom: Atom): Unit = {
    status(atom) = out
    val maybeAtoms: List[Option[Atom]] = justifications(atom) map (findSpoiler(_))
    if (maybeAtoms exists (_.isEmpty)) {
      throw new RuntimeException("could not find spoiler for every SuppRule of atom "+atom)
    }
    Supp(atom) = Set() ++ maybeAtoms map (_.get)
    //SuppRule(a) = None //is not set in beierle [!]
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

  def DDBBeierleOriginal(n: Atom): Unit = {
    if (status(n) != in) return

    //1
    val asms = foundations(n) filter isAssumption
    val maxAssumptions = asms filter { a =>
      ! ((asms - a) exists (b => foundations(b) contains a))
    }
    if (maxAssumptions.isEmpty)
      return //contradiction cannot be solved

    //2
    val na = maxAssumptions.head //culprit
    val nStar = SuppRule(na).get.neg.head //(all .neg have status out at this point)

    //3
    val suppRules = maxAssumptions map (SuppRule(_).get) //J_\bot
    val pos = suppRules flatMap (_.pos) //I_\bot
    val neg = (suppRules flatMap (_.neg)) - nStar //O_\bot
    val rule = AspRuleFromBacktracking(pos, neg, nStar)

    //4
    updateSteps1to5(rule)

    //5
    if (status(n) == in) {
      DDBBeierleOriginal(n) //loop? [1]
    }

  }

  def register(rule: NormalRule): Unit = {
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

  def invalid(rule: NormalRule) = findSpoiler(rule) match {
    case Some(spoiler) => Supp(rule.head) += spoiler; true
    case None => false
  }

  def justifications(h: Atom) = rules filter (_.head == h)

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

  def foundedValid(rule: NormalRule) =
    (rule.pos forall (status(_) == in)) && (rule.neg forall (status(_) == out))

  def foundedInvalid(rule: NormalRule) =
    (rule.pos exists (status(_) == out)) || (rule.neg exists (status(_) == in))

  def unfoundedValid(rule: NormalRule) =
    (rule.pos forall (status(_) == in)) && (!(rule.neg exists (status(_) == in)))

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

  // ----------------- test stuff -------------------

  /** @return true if M is admissible **/
  def set(M: collection.immutable.Set[Atom]): Boolean = { //TODO (HB) Set vs List. Always list for order?
  val m = M.toList
    for (i <- 0 to M.size - 1) {
      val rule: Option[NormalRule] = findSuppRule(m, i)
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
  def findSuppRule(M: List[Atom], idx: Int): Option[NormalRule] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    val rules = justifications(n).filter(rule => rule.pos.subsetOf(MSub) && rule.neg.intersect(M.toSet).isEmpty)
    selectRule(rules)
  }

  def selectRule(rules: List[NormalRule]): Option[NormalRule] = {
    if (rules.isEmpty)
      return None
    Some(rules.head)
  }

}