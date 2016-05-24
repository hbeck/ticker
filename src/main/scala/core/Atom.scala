package core

import core.asp.{AspFact, BuilderHead}
import core.lars.{HeadAtom, Time}

/**
  * Created by hb on 12/22/15.
  */

sealed trait Atom extends HeadAtom {

  def apply(arguments: String*): Atom = {
    AtomWithArguments(this, arguments.toSeq)
  }

  //TODO use implicit instead
  def apply(time: Time): PinnedAtom = {
    PinnedAtom(this, time)
  }

  def arity = 0
}

trait AtomWithArgument extends Atom {
  val atom: Atom

  val arguments: Seq[String]

  override def arity = arguments.size

  override def apply(arguments: String*): Atom = {
    AtomWithArguments(atom, this.arguments ++ arguments)
  }

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

}

case class UserDefinedAtom(caption: String) extends Atom {
  override def toString = caption
}

case class ContradictionAtom(caption: String) extends Atom