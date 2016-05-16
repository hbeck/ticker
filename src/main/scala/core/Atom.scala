package core

import core.asp.{AspFact, AspRule, BuilderHead}
import core.lars.{Time, HeadAtom}

/**
  * Created by hb on 12/22/15.
  */

sealed trait Atom extends HeadAtom {

  def apply(arguments: String*): Atom = {
    AtomWithArguments(this, arguments.toSeq)
  }

  def apply(time: Time): Atom = {
    this.apply(time.toString)
  }

  def arity = 0
}

case class AtomWithArguments(atom: Atom, arguments: Seq[String]) extends Atom {

    override def toString = {
      val sb = new StringBuilder
      sb.append(atom).append("(")

      arguments.addString(sb, ",")

      sb.append(")")

      sb.toString
    }

  override def arity = arguments.size

  def ==(other: AtomWithArguments): Boolean = {
    if (this.atom != other.atom) return false
    if (!this.arguments.equals(other.arguments)) return false
    true
  }

  override def equals(other: Any): Boolean = other match {
    case x: AtomWithArguments => this == x
    case _ => false
  }

  override def apply(arguments: String*): Atom = {
    AtomWithArguments(atom, this.arguments ++ arguments)
  }
}

object Falsum extends Atom

object Atom {


  def unapply(arg: Atom): Option[Seq[String]] = arg match {
    case AtomWithArguments(_, arguments) => Some(arguments)
    case _ => None
  }


  def apply(caption: String): Atom = UserDefinedAtom(caption)

  implicit def headAtomToBuilder(atom: Atom): BuilderHead = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): AspRule = AspFact(atom)

}

case class UserDefinedAtom(caption: String) extends Atom {
  override def toString = caption
}

case class ContradictionAtom(caption: String) extends Atom