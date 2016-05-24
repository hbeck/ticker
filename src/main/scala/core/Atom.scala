package core

import core.asp.{AspFact, BuilderHead}
import core.lars.{ExtendedAtom, HeadAtom, Time}

/**
  * Created by hb on 12/22/15.
  */

sealed trait Atom extends HeadAtom {
  def arity = 0
}

trait AtomWithArgument extends Atom {
  val atom: Atom

  val arguments: Seq[String]

  override def arity = arguments.size

  def ==(other: AtomWithArgument): Boolean = {
    if (this.atom != other.atom) return false
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
}

case class PinnedAtom(timedAtom: Atom, time: Time) extends AtomWithArgument {

  val atom = timedAtom match {
    case aa: AtomWithArgument => aa.atom
    case _ => timedAtom
  }

  override val arguments = timedAtom match {
    case aa: AtomWithArgument => aa.arguments :+ time.toString
    case _ => Seq(time.toString)
  }
}

case class AtomWithArguments(atom: Atom, arguments: Seq[String]) extends AtomWithArgument

object Falsum extends Atom

object Atom {

  def unapply(arg: Atom): Option[Seq[String]] = arg match {
    case aa: AtomWithArgument => Some(aa.arguments)
    case _ => None
  }

  def apply(caption: String): Atom = UserDefinedAtom(caption)

  implicit def headAtomToBuilder(atom: Atom): BuilderHead = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): AspFact[Atom] = AspFact[Atom](atom)

  implicit def toPinnedAtom(atom: Atom): AtomModification = AtomModification(atom)

}

trait AsPinnableAtom {
  val atom: Atom

  def apply(time: Time) = PinnedAtom(atom, time)
}

trait AsAtomWithArgument {
  val atom: Atom

  def apply(arguments: String*): Atom = {
    val appliedArguments: Seq[String] = atom match {
      case AtomWithArguments(_, args) => args ++ arguments.toSeq
      case a: Atom => arguments.toSeq
    }
    AtomWithArguments(atom, appliedArguments)
  }
}

case class AtomModification(atom: Atom) extends AsPinnableAtom with AsAtomWithArgument


case class UserDefinedAtom(caption: String) extends Atom {
  override def toString = caption
}

case class ContradictionAtom(caption: String) extends Atom