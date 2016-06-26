package jtms

import java.util

import core._
import core.asp.{AspRuleFromBacktracking, NormalProgram, NormalRule}

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
case class JtmsBeierle() extends JtmsAbstraction {

  var shuffle = true //debugging

  val suppRule: Map[Atom, Option[NormalRule]] = new HashMap[Atom, Option[NormalRule]]

  override def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = inAtoms()
    if (atoms exists contradictionAtom) return None
    Some(atoms.toSet)
  }

  override def add(rule: NormalRule): Unit = {
    updateSteps1to5(rule)
    //6
    for (n <- allAtoms()) {
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
    if (invalid(rule)) { supp(rule.head) += findSpoiler(rule).get; return }
    //2
    if (ACons(rule.head).isEmpty) {
      setIn(rule)
      return
    }
    //3 (first part)
    val L = repercussions(rule.head) + rule.head

    update(L)
  }

  //extracted at this position for remove case
  override def update(L: Set[Atom]) {

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom,Status,String)]()

    //3 (second part)
    for (atom <- L) {
      step3(atom)
    }
    //4 determine status
    for (atom <- L) {
      step4a(atom)
    }
    //5 fix (choose) status
    step5(L)
  }

  def step3(atom: Atom): Unit = {
    status(atom) = unknown //vs setUnknown [!]
  }

  //determine status
  def step4a(atom: Atom): Unit = {
    if (status(atom) != unknown)
      return

    justifications(atom) find foundedValid match {
      case Some(rule) => {
        setIn(rule)
        for (u <- unknownCons(atom)){
          step4a(u)
        }
      }
      case None => {
        if (justifications(atom) forall foundedInvalid) {
          setOut(atom)
          for (u <- unknownCons(atom)){
            step4a(u)
          }
        }
      }
    }
  }

  def step5(L: Set[Atom]) {

    var atoms = Seq[Atom]() ++ L

    if (doForceChoiceOrder) {
      atoms = (Seq[Atom]() ++ L) sortWith byForcedChoiceSeq
    } else if (shuffle) {
      atoms = shuffleSeq(atoms)
      //println(atoms)
    }

    for (atom <- atoms) {
      step5a(atom)
    }

  }

  def shuffleSeq(atoms: Seq[Atom]): Seq[Atom] = {
    val list = new util.ArrayList[Atom]()
    atoms foreach list.add
    java.util.Collections.shuffle(list)
    var seq = Seq[Atom]()
    for (i <- 0 to list.size()-1) {
      seq = seq :+ list.get(i)
    }
    seq
  }

  def byForcedChoiceSeq(a:Atom, b:Atom): Boolean = {
    val aIdx = forcedChoiceSeq.indexOf(a)
    val bIdx = forcedChoiceSeq.indexOf(b)
    if (aIdx == -1) return false
    if (bIdx == -1) return true
    aIdx <= bIdx
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
          setIn(rule) //TODO log as "fix"
          for (n <- rule.neg) {
            if (status(n) == unknown) {
              status(n) = out //vs setOutOriginal [!]; support never set!
            }
          }
          for (u <- unknownCons(atom)) { //* here other variant is chosen. deliberately? [1]
            step5a(u)
          }
        }
      }
      case None => { //all justifications(atom) are unfounded invalid
        status(atom)=out
        for (rule <- justifications(atom)) {
          val n: Option[Atom] = rule.pos find (status(_) == unknown) //in general, rule.pos might be empty! [!]
          if (n.isEmpty) {
            throw new RuntimeException("did not find rule.pos atom with status unknown in rule "+rule+" for atom "+atom)
          }
        }
        setOut(atom)
        for (u <- unknownCons(atom)) {
          step5a(u)
        }
      }
    }
  }

  //TODO there's no such method in the book!
  override def setIn(rule: NormalRule) = {

    if (recordStatusSeq) statusSeq = statusSeq :+ (rule.head,in,"set")

    status(rule.head) = in
    supp(rule.head) = Set() ++ rule.body
    suppRule(rule.head) = Some(rule)
  }

  //TODO there's no such method in the book!
  override def setOut(atom: Atom): Unit = {

    if (recordStatusSeq) statusSeq = statusSeq :+ (atom,out,"set")

    status(atom) = out
    setOutSupport(atom)

    //SuppRule(a) = None //is not set in beierle [!]
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
    val nStar = suppRule(na).get.neg.head //(all .neg have status out at this point)

    //3
    val suppRules = maxAssumptions map (suppRule(_).get) //J_\bot
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

  override def register(a: Atom): Unit = {
    super.register(a)
    if (!suppRule.isDefinedAt(a)) suppRule(a) = None
  }

  //ACons(a) = {x ∈ Cons(a) | a ∈ Supp(x)}
  def ACons(a: Atom): Set[Atom] = cons(a) filter (supp(_) contains a)

  def isAssumption(a: Atom) = (status(a) == in) && !suppRule(a).get.neg.isEmpty

  def foundedValid(rule: NormalRule) = valid(rule)

  def foundedInvalid(rule: NormalRule) = invalid(rule)

  def unfoundedValid(rule: NormalRule) = posValid(rule)

  //

  override def unregister(a: Atom): Unit = {
    super.unregister(a)
    suppRule remove a
  }

  override def setInSupport(a: Atom) = justifications(a) find foundedValid match {
    case Some(rule) => {
      supp(a) = Set() ++ rule.body
      suppRule(a) = Some(rule)
    }
    case _ => throw new IncrementalUpdateFailureException()
  }

  override def setOutSupport(a: Atom) {
    super.setOutSupport(a)
    suppRule(a) = None //not set in beierle, but relevant only for backtracking
  }

  // -- from refactored implementation, not in use --

  //return true if method leaves with status(c) != in
  def DDBRefactored(c: Atom): Boolean = {

    if (status(c) != in) return true

    val asms = foundations(c) filter isAssumption
    val maxAssumptions = asms filter { a =>
      ! ((asms - a) exists (b => foundations(b) contains a))
    }

    if (maxAssumptions.isEmpty)
      return false //contradiction cannot be solved

    findBacktrackingRule(maxAssumptions) match {
      case Some(rule) => add(rule); return true
      case None => return false
    }

  }

  def findBacktrackingRule(maxAssumptions: Set[Atom]): Option[AspRuleFromBacktracking] = {

    val culprit = maxAssumptions.head
    val n = suppRule(culprit).get.neg.head //(all .neg have status out at this point)

    val suppRules = maxAssumptions map (suppRule(_).get)
    val pos = suppRules flatMap (_.pos)
    val neg = (suppRules flatMap (_.neg)) - n
    val rule = AspRuleFromBacktracking(pos, neg, n)

    Some(rule)
  }

}