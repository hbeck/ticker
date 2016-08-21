package core

import core.asp.{AspFact, BuilderHead}
import core.lars._

/**
  * Created by hb on 12/22/15.
  */

sealed trait Atom extends HeadAtom {
  def arity = 0

  def isGround(): Boolean

  /*
  //convenience for testing TODO review
  def apply(int: Int): Atom = PinnedAtom(this,TimePoint(int))

  //TODO
  def apply(s: String): Atom = Predicate(s)
  */
}

trait GroundAtom extends Atom {
  override def isGround(): Boolean = true
  override def assign(assignment: Assignment) = this
}

trait AtomWithArgument extends Atom {
  val atom: Atom //TODO Atom vs Predicate !?

  val arguments: Seq[Argument]

  override def arity = arguments.size

  def ==(other: AtomWithArgument): Boolean = {
    if (this eq other) return true
    if (this.atom != other.atom) return false
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

    sb.append(atom).append("(")

    arguments.addString(sb, ",")

    sb.append(")")

    sb.toString
  }

  override def isGround(): Boolean = arguments forall (s => s.isInstanceOf[Value])

}

case class Predicate(caption: String) extends GroundAtom {
  override def toString = caption

  override def isGround(): Boolean = true
}

object Falsum extends GroundAtom

case class ContradictionAtom(caption: String) extends GroundAtom

case class NonGroundAtom(atom: Atom, arguments: Seq[Argument]) extends AtomWithArgument {
  override def assign(assignment: Assignment): AtomWithArgument = {
    val newArguments = arguments map { arg =>
      assignment(arg) match {
        case Some(value) => value
        case _ => arg
      }
    }
    NonGroundAtom(atom, newArguments)
  }
}

case class GroundAtomWithArguments(atom: Atom, arguments: Seq[Value]) extends GroundAtom with AtomWithArgument {
  override def isGround() = true
}

object GroundAtom {
  //TODO type predicate
  def apply(atom: Atom, arguments: Value*): GroundAtomWithArguments = GroundAtomWithArguments(atom, arguments.toList)
}

case class PinnedAtom(timedAtom: Atom, time: Time) extends AtomWithArgument {

  val atom = timedAtom match {
    case aa: AtomWithArgument => aa.atom
    case _ => timedAtom
  }

  val timeAsArgument: Argument = time match {
    case TimePoint(t) => Value(t)
    case t: TimeVariableWithOffset => Variable(t.toString)
  }

  override val arguments = timedAtom match {
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

  def apply(caption: String): Atom = Predicate(caption)

  implicit def headAtomToBuilder(atom: Atom): BuilderHead = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): AspFact[Atom] = AspFact[Atom](atom)

  implicit def asAtomModification(atom: Atom): AtomModification = AtomModification(atom)
}

