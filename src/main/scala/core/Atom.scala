package core

/**
  * Created by hb on 12/22/15.
  */
sealed trait Atom

object Falsum extends Atom

object Atom {
  def apply(caption: String): Atom = UserDefinedAtom(caption)

  implicit def headAtomToBuilder(atom: Atom) = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): Rule = Fact(atom)

}

case class UserDefinedAtom(caption: String) extends Atom {
  override def toString = caption
}

case class ContradictionAtom(caption: String) extends Atom