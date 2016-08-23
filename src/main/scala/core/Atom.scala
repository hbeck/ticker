package core

import core.asp.{AspFact, BuilderHead}
import core.lars._

/**
  * Created by hb on 12/22/15.
  */
sealed trait Atom extends HeadAtom {

  override val atom = this

  def arity = 0

  def isGround(): Boolean

  val predicate: Predicate

  //override def atom(): Atom = this

  /*
  //convenience for testing TODO review
  def apply(int: Int): Atom = PinnedAtom(this,TimePoint(int))

  //TODO
  def apply(s: String): Atom = Predicate(s)
  */

}

case class Predicate(caption: String) {
  override def toString = caption
}

trait GroundAtom extends Atom {
  override def isGround(): Boolean = true

  override def assign(assignment: Assignment) = this
}

object Falsum extends GroundAtom {
  override val predicate = Predicate("⊥")
}

case class ContradictionAtom(caption: String) extends GroundAtom {
  override val predicate = Predicate(caption)
}

// TODO: should we use FactAtom? We need this as a wrapper around an Atom consisting only of a Predicate and no Arguments
case class PredicateAtom(predicate: Predicate) extends GroundAtom {
  override def toString = predicate.toString
}

trait AtomWithArgument extends Atom {

  val arguments: Seq[Argument]

  override def arity = arguments.size

  def ==(other: AtomWithArgument): Boolean = {
    if (this eq other) return true
    if (this.predicate != other.predicate) return false
    if (this.arguments.length != other.arguments.length) return false
    if (!this.arguments.equals(other.arguments)) return false
    true
  }

  override def equals(other: Any): Boolean = other match {
    case x: AtomWithArgument => this == x
    case _ => false
  }

  override def toString = {
    val sb = new StringBuilder
    sb.append(predicate).append("(")
    arguments.addString(sb, ",")
    sb.append(")")
    sb.toString
  }

  override def isGround(): Boolean = arguments forall (s => s.isInstanceOf[Value])

}

object AtomWithArgument {
  def apply(predicate: Predicate, arguments: Seq[Argument]): AtomWithArgument = arguments.forall(_.isInstanceOf[Value]) match {
    case true => GroundAtomWithArguments(predicate, arguments.map(_.asInstanceOf[Value]).toList)
    case false => NonGroundAtom(predicate, arguments)
  }
}


case class NonGroundAtom(override val predicate: Predicate, arguments: Seq[Argument]) extends AtomWithArgument {
  override def assign(assignment: Assignment): AtomWithArgument = {
    val newArguments = arguments map { arg =>
      assignment(arg) match {
        case Some(value) => value
        case _ => arg
      }
    }
    NonGroundAtom(predicate, newArguments)
  }
}

case class GroundAtomWithArguments(override val predicate: Predicate, arguments: Seq[Value]) extends GroundAtom with AtomWithArgument {
  override def isGround() = true
}

object GroundAtom {
  def apply(predicate: Predicate, arguments: Value*): GroundAtom = {
    if (arguments.isEmpty)
      PredicateAtom(predicate)
    else
      GroundAtomWithArguments(predicate, arguments.toList)
  }
}


case class PinnedAtom(override val atom: Atom, time: Time) extends AtomWithArgument {

  override val predicate = atom match {
    case aa: AtomWithArgument => aa.predicate
    case _ => atom.predicate
  }

  val timeAsArgument: Argument = time match {
    case TimePoint(t) => Value(t)
    case t: TimeVariableWithOffset => Variable(t.toString)
  }

  override val arguments = atom match {
    case aa: AtomWithArgument => aa.arguments :+ timeAsArgument
    case _ => Seq(timeAsArgument)
  }

  override def isGround(): Boolean = timeAsArgument.isInstanceOf[Value]

  //assume pinned atoms may have variables only in its special time argument
  override def assign(assignment: Assignment): ExtendedAtom = this
}


object Atom {

  def unapply(atom: Atom): Option[Seq[Argument]] = atom match {
    case aa: AtomWithArgument => Some(aa.arguments)
    case _ => None
  }

  def apply(caption: String): Atom = PredicateAtom(Predicate(caption))

  def apply(predicate: Predicate, arguments: Seq[Argument]) = arguments.forall(_.isInstanceOf[Value]) match {
    case true => GroundAtom(predicate, arguments.map(_.asInstanceOf[Value]).toList: _*)
    case false => NonGroundAtom(predicate, arguments)
  }

  implicit def headAtomToBuilder(atom: Atom): BuilderHead = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): AspFact[Atom] = AspFact[Atom](atom)

  implicit def asAtomModification(atom: Atom): AtomModification = AtomModification(atom)
}

