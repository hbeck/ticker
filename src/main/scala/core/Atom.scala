package core

/**
  * Created by hb on 12/22/15.
  */
// TODO: sealed was temporary removed - TBD is this OK?
 trait Atom {

  def apply(arguments: String*): Atom = {
    AtomWithArguments(this, arguments.toSeq)
  }

  def arity = 0
}

case class AtomWithArguments(atom: Atom, arguments: Seq[String]) extends Atom {

  //  override def toString = {
  //    val sb = new StringBuilder
  //    sb.append(atom).append("(")
  //
  //    arguments.addString(sb, ",")
  //
  //    sb.append(")")
  //
  //    sb.toString
  //  }

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

  def apply(caption: String): Atom = UserDefinedAtom(caption)

  implicit def headAtomToBuilder(atom: Atom): BuilderHead = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): AspRule = AspFact(atom)

}

case class UserDefinedAtom(caption: String) extends Atom {
  override def toString = caption
}

case class ContradictionAtom(caption: String) extends Atom