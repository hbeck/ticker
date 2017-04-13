package jtms.algorithms

import core.Atom
import core.asp._
import jtms._

import scala.util.Random

/**
  * Created by FM on 13.10.16.
  */
abstract class JtmsUpdateAlgorithmAbstraction(network: TruthMaintenanceNetwork, random: Random) extends JtmsUpdateAlgorithm {

  def rules = network.rules

  def findSpoiler(rule: NormalRule): Option[Atom] = {
    if (random.nextDouble() < 0.5) {
      rule.pos find (network.status(_) == out) match {
        case None => rule.neg find (network.status(_) == in)
        case opt => opt
      }
    } else {
      rule.neg find (network.status(_) == in) match {
        case None => rule.pos find (network.status(_) == out)
        case opt => opt
      }
    }
  }

  def findStatus(a: Atom): Unit = {
    if (network.status(a) != unknown) return

    if (validation(a) || invalidation(a)) {
      network.unknownCons(a) foreach findStatus
    }
  }

  def validation(a: Atom): Boolean = {
    network.justifications(a) find network.valid match {
      case Some(rule) => setIn(rule); true
      case None => false
    }
  }

  def invalidation(a: Atom): Boolean = {
    if (network.justifications(a) forall network.invalid) {
      setOut(a)
      return true
    }
    false
  }

  def invalidateModel(): Unit = {
    network.atomsNeedingSupp foreach setUnknown
  }

  def register(rule: NormalRule): Boolean = {
    if (network.register(rule)) {
      if (recordStatusSeq) {
        val atoms = network.outAtoms
        val newOutAtoms = network.outAtoms diff atoms
        newOutAtoms foreach { a =>
          statusSeq = statusSeq :+ (a, out, "register")
        }
      }
      return true
    }
    false
  }

  def unregister(rule: NormalRule) = network.unregister(rule)

  //TODO recompute only old and current repercussions in case of inconsistency
  //based on JTMS update algorithm
  override def add(rule: NormalRule): Unit = {
    register(rule)
    if (network.inconsistent) {
      update(network.unknownAtoms + rule.head) //i.e., recompute()
    } else {
      if (network.status(rule.head) == in) {
        if (network.valid(rule)) {
          // TODO: this could create self-support!
          // Scenario:
          // dz is in, support is: dz :- some, w_te_2_d_z.
          // new rule is added: dz :- dz_at(18).
          // is already valid, creates self-support :-/

          // TODO: do a benchmark here
          val foundations = rule.body flatMap network.foundations
          if (!foundations.contains(rule.head))
          //difference to original; optimization for sliding time-based window (support always by latest)
            setIn(rule)
        }
        return
      }
      if (network.invalid(rule)) {
        //supp(rule.head) += findSpoiler(rule).get; return
        network.addSupport(rule.head, findSpoiler(rule).get)
        return
      }
      val atoms = network.repercussions(rule.head) + rule.head
      update(atoms)
    }
  }

  override def remove(rule: NormalRule): Unit = {
    unregister(rule)
    if (network.inconsistent) {
      val h = if (network.allAtoms contains rule.head) Set(rule.head) else Set()
      update(network.unknownAtoms ++ h)
    } else {
      if (!(network.allAtoms contains rule.head)) return
      if (network.status(rule.head) == out) return
      //this should save some time!:
      if (network.suppRule(rule.head).isDefined && network.suppRule(rule.head).get != rule) return
      //.isDefined needed if previous state was inconsistent
      val atoms = network.repercussions(rule.head) + rule.head
      update(atoms)
    }
  }

  def recompute(): Unit = {
    update(network.unknownAtoms)
  }

  override def getModel(): Option[scala.collection.immutable.Set[Atom]] = {
    val atoms = network.inAtoms
    if (atoms exists network.contradictionAtom) return None //not dealt with; left for old test-cases
    if (network.hasUnknown) return None
    Some(atoms)
  }

  override def set(model: collection.immutable.Set[Atom]): Boolean = {
    invalidateModel()
    //model foreach (status(_) = in)
    model foreach { atom =>
      //status = status.updated(atom,in)
      network.updateStatus(atom, in)
    }
    //(allAtoms diff model) foreach (status(_) = out)
    (network.allAtoms diff model) foreach { atom =>
      //status = status.updated(atom,out)
      network.updateStatus(atom, out)
    }
    try {
      network.atomsNeedingSupp foreach setSupport
    } catch {
      case e: IncrementalUpdateFailureException => {
        invalidateModel()
        return false
      }
    }
    true
  }


  def setSupport(a: Atom) {
    network.status(a) match {
      case `in` => setInSupport(a)
      case `out` => setOutSupport(a)
      case `unknown` => network.clearSupport(a) //supp(a) = Set()
    }
  }

  def setInSupport(a: Atom) = network.justifications(a) find network.valid match {
    case Some(rule) => {
      network.setInSupport(a, rule)
      assert(a == rule.head)
      //      jtms.setSupport(a, rule.body) //supp(a) = Set() ++ rule.body
      //      jtms.suppRule = jtms.suppRule.updated(rule.head, Some(rule))
    }
    case _ => throw new IncrementalUpdateFailureException()
  }

  def setOutSupport(a: Atom) {
    val maybeAtoms: Set[Option[Atom]] = network.justifications(a) map findSpoiler
    if (maybeAtoms exists (_.isEmpty)) {
      throw new IncrementalUpdateFailureException("could not find spoiler for every justification of atom " + a)
    }
    //supp(a) = Set() ++ maybeAtoms map (_.get)
    //    jtms.setSupport(a, maybeAtoms map (_.get))
    network.setOutSupport(a, maybeAtoms map (_.get))
  }

  def setIn(rule: NormalRule) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (rule.head, in, "set")
    network.updateStatus(rule.head, in)
    network.setInSupport(rule.head, rule)
    //    jtms.suppRule = jtms.suppRule.updated(rule.head, Some(rule))
    //    status(rule.head) = in
    //    supp(rule.head) = Set() ++ rule.body
    //    suppRule(rule.head) = Some(rule)
  }

  def setOut(a: Atom) = {
    if (recordStatusSeq) statusSeq = statusSeq :+ (a, out, "set")
    network.updateStatus(a, out)
    setOutSupport(a)
    //    jtms.suppRule = jtms.suppRule.updated(a, None)
    //    status(a) = out
    //    setOutSupport(a)
    //    suppRule(a) = None
  }

  def setUnknown(a: Atom) = {
    network.updateStatus(a, unknown)
    network.clearSupport(a)
    //    jtms.suppRule = jtms.suppRule.updated(a, None)
    //    status(a) = unknown
    //    supp(a) = Set()
    //    suppRule(a) = None
  }
}
