package core

/**
  * Created by hb on 12/22/15.
  */
sealed trait Atom

object Falsum extends Atom

object Atom {
  def apply(caption: String): Atom = NamedAtom(caption)

  implicit def headAtomToBuilder(atom: Atom) = new BuilderHead(atom)
}

case class NamedAtom(caption: String) extends Atom