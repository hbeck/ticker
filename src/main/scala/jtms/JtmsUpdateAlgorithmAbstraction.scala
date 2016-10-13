package jtms

import core.Atom
import core.asp._

import scala.util.Random

/**
  * Created by FM on 13.10.16.
  */
abstract class JtmsUpdateAlgorithmAbstraction(jtms: JtmsAbstraction, random: Random) extends JtmsUpdateAlgorithm {


  def findSpoiler(rule: NormalRule): Option[Atom] = {
    if (random.nextDouble() < 0.5) {
      rule.pos find (jtms.status(_) == out) match {
        case None => rule.neg find (jtms.status(_) == in)
        case opt => opt
      }
    } else {
      rule.neg find (jtms.status(_) == in) match {
        case None => rule.pos find (jtms.status(_) == out)
        case opt => opt
      }
    }
  }

  def findStatus(a: Atom): Unit = {
    if (jtms.status(a) != unknown)
      return

    if (validation(a) || invalidation(a))
      jtms.unknownCons(a) foreach findStatus
  }

  def validation(a: Atom): Boolean = {
    jtms.justifications(a) find jtms.valid match {
      case Some(rule) => setIn(rule); true
      case None => false
    }
  }

  def invalidation(a: Atom): Boolean = {
    if (jtms.justifications(a) forall jtms.invalid) {
      setOut(a)
      return true
    }
    false
  }

  def invalidateModel(): Unit = {
    jtms.atomsNeedingSupp foreach setUnknown
  }

  def register(rule: NormalRule) = jtms.register(rule)

  def unregister(rule: NormalRule) = jtms.unregister(rule)

  //based on JTMS update algorithm
  override def add(rule: NormalRule): Unit = {
    register(rule)
    if (jtms.inconsistent()) {
      update(jtms.unknownAtoms() + rule.head) //i.e., recompute()
    } else {
      if (jtms.status(rule.head) == in) {
        if (jtms.valid(rule)) {
          //difference to original; optimization for sliding time-based window (support always by latest)
          setIn(rule)
        }
        return
      }
      if (jtms.invalid(rule)) {
        //supp(rule.head) += findSpoiler(rule).get; return
        jtms.addSupport(rule.head, findSpoiler(rule).get)
        return
      }
      val atoms = jtms.repercussions(rule.head) + rule.head
      update(atoms)
    }
  }

  override def remove(rule: NormalRule): Unit = {
    unregister(rule)
    if (jtms.inconsistent()) {
      val h = if (jtms.allAtoms contains rule.head) Set(rule.head) else Set()
      update(jtms.unknownAtoms() ++ h)
    } else {
      if (!(jtms.allAtoms contains rule.head)) return
      if (jtms.status(rule.head) == out) return
      //this should save some time!:
      if (jtms.suppRule(rule.head).isDefined && jtms.suppRule(rule.head).get != rule) return //.isDefined needed if previous state was inconsistent
      val atoms = jtms.repercussions(rule.head) + rule.head
      update(atoms)
    }
  }

  def recompute(): Unit = {
    update(jtms.unknownAtoms())
  }

  override def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = jtms.inAtoms()
    if (atoms exists jtms.contradictionAtom) return None //not dealt with; left for old test-cases
    if (jtms.hasUnknown()) return None
    Some(atoms)
  }


  override def set(model: collection.immutable.Set[Atom]): Boolean = {
    invalidateModel()
    //model foreach (status(_) = in)
    model foreach { atom =>
      //status = status.updated(atom,in)
      jtms.__updateStatus(atom, in)
    }
    //(allAtoms diff model) foreach (status(_) = out)
    (jtms.allAtoms diff model) foreach { atom =>
      //status = status.updated(atom,out)
      jtms.__updateStatus(atom, out)
    }
    try {
      jtms.atomsNeedingSupp() foreach setSupport
    } catch {
      case e: IncrementalUpdateFailureException => {
        invalidateModel()
        return false
      }
    }
    true
  }


  def setSupport(a: Atom) {
    jtms.status(a) match {
      case `in` => setInSupport(a)
      case `out` => setOutSupport(a)
      case `unknown` => jtms.clearSupport(a) //supp(a) = Set()
    }
  }

  def setInSupport(a: Atom) = jtms.justifications(a) find jtms.valid match {
    case Some(rule) => jtms.setSupport(a, rule.body) //supp(a) = Set() ++ rule.body
    case _ => throw new IncrementalUpdateFailureException()
  }

  def setOutSupport(a: Atom) {
    val maybeAtoms: Set[Option[Atom]] = jtms.justifications(a) map findSpoiler
    if (maybeAtoms exists (_.isEmpty)) {
      throw new IncrementalUpdateFailureException("could not find spoiler for every justification of atom " + a)
    }
    //supp(a) = Set() ++ maybeAtoms map (_.get)
    jtms.setSupport(a, maybeAtoms map (_.get))
  }

  def setIn(rule: NormalRule) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rule.head, in, "set")
    jtms.__updateStatus(rule.head, in)
    jtms.setSupport(rule.head, rule.body)
    jtms.suppRule = jtms.suppRule.updated(rule.head, Some(rule))
    //    status(rule.head) = in
    //    supp(rule.head) = Set() ++ rule.body
    //    suppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (a, out, "set")
    jtms.__updateStatus(a, out)
    setOutSupport(a)
    jtms.suppRule = jtms.suppRule.updated(a, None)
    //    status(a) = out
    //    setOutSupport(a)
    //    suppRule(a) = None
  }

  def setUnknown(a: Atom) = {
    jtms.__updateStatus(a, unknown)
    jtms.clearSupport(a)
    jtms.suppRule = jtms.suppRule.updated(a, None)
    //    status(a) = unknown
    //    supp(a) = Set()
    //    suppRule(a) = None
  }
}
