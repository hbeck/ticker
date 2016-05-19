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

  def apply(time: Time): AtomWithTime = {
    AtomWithTime(this, time)
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

case class AtomWithTime(timedAtom: Atom, time: Time) extends AtomWithArgument {

  val atom = timedAtom match {
    case aa: AtomWithArgument => aa.atom
    case _ => timedAtom
  }

  override def apply(time: Time): AtomWithTime = {
    // TODO: We currently only model the last time-parameter explicitly. Guess this is enough?
    //    AtomWithTime(AtomWithArguments(this.atom, Seq(this.time.toString())), time)
    AtomWithTime(this, time)
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

  implicit def headAtomToFact(atom: Atom): AspRule = AspFact(atom)

}

case class UserDefinedAtom(caption: String) extends Atom {
  override def toString = caption
}

case class ContradictionAtom(caption: String) extends Atom