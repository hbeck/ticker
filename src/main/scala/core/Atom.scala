package core

import core.asp.{AspFact, BuilderHead}
import core.lars.{HeadAtom, Time, TimePoint}

/**
  * Created by hb on 12/22/15.
  */

sealed trait Atom extends HeadAtom {
  def arity = 0

  def isGround(): Boolean = true
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

  override def isGround(): Boolean = {
    arguments forall (s => s.isEmpty || s.charAt(0).isLower)
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

  override def isGround(): Boolean = time.isInstanceOf[TimePoint]
}

case class AtomWithArguments(atom: Atom, arguments: Seq[String]) extends AtomWithArgument

case class AtomWithVariables(atom: Atom, variables: Seq[Variable], otherArguments: Seq[String]) extends AtomWithArgument {
  val arguments = otherArguments ++ variables.map(_.name)

  override def isGround() = variables.isEmpty
}

object Falsum extends Atom {
  override def isGround = true
}

object Atom {

  def unapply(arg: Atom): Option[Seq[String]] = arg match {
    case aa: AtomWithArgument => Some(aa.arguments)
    case _ => None
  }

  def apply(caption: String): Atom = Predicate(caption)

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

  def variableArguments(arguments: Variable*): Atom = {
    val otherArguments: Seq[String] = atom match {
      case AtomWithArguments(_, args) => args
      case a: Atom => Seq()
    }
    AtomWithVariables(atom, arguments, otherArguments)
  }

  def genericArguments(arguments: String*): Atom = {
    val appliedArguments: Seq[String] = atom match {
      case AtomWithArguments(_, args) => args ++ arguments.toSeq
      case a: Atom => arguments.toSeq
    }
    AtomWithArguments(atom, appliedArguments)
  }

  def apply(arguments: Any*): Atom = {
    // TODO: can this go into pattern matching?
    if (arguments.forall(_.isInstanceOf[Variable]))
      return variableArguments(arguments map (_.asInstanceOf[Variable]): _*)
    else if (arguments.forall(_.isInstanceOf[String])) {
      val args = arguments.map(_.asInstanceOf[String])
      if (args.forall(_.head.isUpper))
        return variableArguments(args map (Variable(_)): _*)
      else
        return genericArguments(args: _*)
    }
    throw new IllegalArgumentException("Don't know how to handle generic arguments")
  }

  def genericArguments(arguments: String*): Atom = {
    val appliedArguments: Seq[String] = atom match {
      case AtomWithArguments(_, args) => args ++ arguments.toSeq
      case a: Atom => arguments.toSeq
    }
    AtomWithArguments(atom, appliedArguments)
  }
}

case class AtomModification(atom: Atom) extends AsPinnableAtom with AsAtomWithArgument


case class Predicate(caption: String) extends Atom {
  override def toString = caption
}

case class ContradictionAtom(caption: String) extends Atom