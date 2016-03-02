package core

/**
  * Created by hb on 12/22/15.
  */
sealed trait Atom

object Falsum extends Atom

sealed trait AtomWithName extends Atom{
  def caption: String
}

object Atom {
  def apply(caption: String): Atom = NamedAtom(caption)
}

case class NamedAtom(caption: String) extends AtomWithName