package core

/**
  * Created by hb on 12/22/15.
  */
sealed trait Atom {
  def caption: String
}

object Atom {
  def apply(caption: String): Atom = UserDefinedAtom(caption)
}

case class UserDefinedAtom(caption: String) extends Atom

case class ContradictionAtom(caption: String) extends Atom