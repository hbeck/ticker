package core

import core.asp.{AspFact, BuilderHead}
import core.lars.{HeadAtom, Time, TimePoint, TimeVariableWithOffset}

/**
  * Created by hb on 12/22/15.
  */

sealed trait Atom extends HeadAtom {
  def arity = 0

  def isGround(): Boolean = true
}

trait AtomWithArgument extends Atom {
  val atom: Atom

  val arguments: Seq[Argument]

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

  override def isGround(): Boolean = {
    arguments forall (s => s.isInstanceOf[Value])
  }

}

case class Predicate(caption: String) extends Atom {
  override def toString = caption
}

object Falsum extends Atom

case class ContradictionAtom(caption: String) extends Atom

case class AtomWithArguments(atom: Atom, arguments: Seq[Argument]) extends AtomWithArgument

case class GroundAtom(atom: Atom, arguments: List[Value] = List()) extends AtomWithArgument {
  override def isGround() = true
}

object GroundAtom {//TODO type predicate
  def apply(predicate: Atom, arguments: Value*): GroundAtom = GroundAtom(predicate, arguments.toList)
}

case class PinnedAtom(timedAtom: Atom, time: Time) extends AtomWithArgument {

  val atom = timedAtom match {
    case aa: AtomWithArgument => aa.atom
    case _ => timedAtom
  }

  val timeAsArgument: Argument = time match {
    case TimePoint(t) => Value(t.toString)
    case t: TimeVariableWithOffset => Variable(t.toString)
  }

  override val arguments = timedAtom match {
    case aa: AtomWithArgument => aa.arguments :+ timeAsArgument
    case _ => Seq(timeAsArgument)
  }

  override def isGround(): Boolean = timeAsArgument.isInstanceOf[Value]
}


object Atom {

  def unapply(arg: Atom): Option[Seq[Argument]] = arg match {
    case aa: AtomWithArgument => Some(aa.arguments)
    case _ => None
  }

  def apply(caption: String): Atom = Predicate(caption)

  implicit def headAtomToBuilder(atom: Atom): BuilderHead = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): AspFact[Atom] = AspFact[Atom](atom)

  implicit def asAtomModification(atom: Atom): AtomModification = AtomModification(atom)
}

